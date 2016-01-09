%% Copyright (c) 2012-2015 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(haystack_rr_tsig).
-export([decode/3]).
-export([encode/4]).

-on_load(on_load/0).

on_load() ->
    haystack_table:reuse(?MODULE).

%%
%% https://tools.ietf.org/html/rfc2845
%%


decode(Resource, Data, Packet) ->
    decode(haystack_protocol:qr(Packet), Resource, Data, Packet).

decode(QR, Resource, Data, Packet) ->
    {Name, _} = haystack_name:decode(Resource, Packet),
    {Algorithm, <<
                  TimeSigned:48,
                  Fudge:16,
                  MACSize:16,
                  MAC:MACSize/bytes,
                  OriginalID:16,
                  Error:16,
                  OtherLen:16,
                  OtherData:OtherLen/bytes
                >>} = haystack_name:decode(Data, Packet),

    %% Upon receipt of a message with a correctly placed TSIG RR, the
    %% TSIG RR is copied to a safe location, removed from the DNS
    %% Message, and decremented out of the DNS message header's
    %% ARCOUNT. At this point the keyed message digest operation is
    %% performed.
    %%
    [Pristine, <<>>] = binary:split(Packet, Resource),
    decode(QR, reduce_arcount(Pristine), Name, Algorithm,
           haystack_date:to_datetime(TimeSigned), Fudge, MAC, OriginalID,
           haystack_rcode:lookup(Error), OtherData).



decode(response, Pristine, Name, Algorithm, TimeSigned,
       Fudge, MAC, OriginalID, Error, OtherData) ->
    Id = haystack_protocol:id(Pristine),
    Digest = case ets:take(?MODULE, Id) of
                 [RequestMAC] ->
                     <<
                       (byte_size(mac(RequestMAC))):16,
                       (mac(RequestMAC))/binary,
                       Pristine/binary,
                       (variables(Name,
                                  Algorithm,
                                  TimeSigned,
                                  Fudge,
                                  Error,
                                  OtherData))/binary
                     >>;
                 [] ->
                     error({badarg, Id})
             end,
    check_mac(Name, Algorithm, TimeSigned, Fudge, MAC, OriginalID, Error,
              OtherData, Digest);

decode(_QR, Pristine, Name, Algorithm, TimeSigned,
       Fudge, MAC, OriginalID, Error, OtherData) ->

    Digest = <<
               Pristine/binary,
               (variables(Name,
                          Algorithm,
                          TimeSigned,
                          Fudge,
                          Error,
                          OtherData))/binary
             >>,

    check_mac(Name, Algorithm, TimeSigned, Fudge, MAC, OriginalID, Error,
              OtherData, Digest).



check_mac(Name, Algorithm, TimeSigned, Fudge, MAC, OriginalID, Error,
          OtherData, Digest) ->

    Elapsed = haystack_date:seconds_elapsed_since(TimeSigned),

    case hash(Algorithm, key(Name), Digest) of

        {ok, MAC} when Elapsed =< Fudge ->
            #{algorithm => Algorithm,
              time_signed => TimeSigned,
              name => Name,
              fudge => Fudge,
              mac => MAC,
              original_id => OriginalID,
              error => Error,
              other_data => OtherData};

        {ok, MAC} ->
            #{algorithm => Algorithm,
              time_signed => TimeSigned,
              name => Name,
              fudge => Fudge,
              mac => MAC,
              original_id => OriginalID,
              error => bad_time,
              other_data => OtherData};

        {ok, _} ->
            #{algorithm => Algorithm,
              time_signed => TimeSigned,
              name => Name,
              fudge => Fudge,
              original_id => OriginalID,
              error => bad_sig,
              other_data => OtherData};

        {error, HashError} ->
            #{algorithm => Algorithm,
              time_signed => TimeSigned,
              name => Name,
              fudge => Fudge,
              original_id => OriginalID,
              error => HashError,
              other_data => OtherData}
    end.



encode(TSIG, Pristine, WithHeader, Offsets) ->
    encode(haystack_protocol:qr(Pristine), TSIG, Pristine, WithHeader, Offsets).


%% 4.3. TSIG on TSIG Error returns
%%
%%   When a server detects an error relating to the key or MAC, the server
%%   SHOULD send back an unsigned error message (MAC size == 0 and empty
%%   MAC).  If an error is detected relating to the TSIG validity period,
%%   the server SHOULD send back a signed error message.  The digest
%%   components are:
%%
%%      Request MAC (if the request MAC validated)
%%      DNS Message (response)
%%      TSIG Variables (response)
%%
%%   The reason that the request is not included in this digest in some
%%   cases is to make it possible for the client to verify the error.  If
%%   the error is not a TSIG error the response MUST be generated as
%%   specified in [4.2].

encode(response, #{algorithm := Algorithm,
                   original_id := OriginalID,
                   error := Error,
                   other_data := OtherData},
       _,
       Packet1,
       Offsets1) when Error == bad_sig orelse
                      Error == bad_key ->
    {Packet2, Offsets2} = haystack_name:encode(Algorithm, Packet1, Offsets1),
    {<<
       Packet2/binary,
       (rdata(<<>>, OriginalID, Error, OtherData))/binary
     >>, Offsets2};


%% 4.2. TSIG on Answers
%%
%%   When a server has generated a response to a signed request, it signs
%%   the response using the same algorithm and key.  The server MUST not
%%   generate a signed response to an unsigned request.  The digest
%%   components are:
%%
%%      Request MAC
%%      DNS Message (response)
%%      TSIG Variables (response)

encode(response, #{algorithm := Algorithm,
                   mac := RequestMAC,
                   name := Name,
                   original_id := OriginalID,
                   error := Error,
                   other_data := OtherData}, Pristine, WithHeader, Offsets1) ->

    TimeSigned = erlang:universaltime(),
    Fudge = haystack_config:tsig_rr_fudge(),
    Digest = <<
               (byte_size(RequestMAC)):16,
               RequestMAC/binary,
               (reduce_arcount(Pristine))/binary,
               (variables(Name,
                          Algorithm,
                          TimeSigned,
                          Fudge,
                          Error,
                          OtherData))/binary
             >>,
    {ok, ResponseMAC} = hash(Algorithm, key(Name), Digest),
    {Packet2, Offsets2} = haystack_name:encode(Algorithm, WithHeader, Offsets1),

    %% 4.1. TSIG generation on requests
    %%
    %%   Client performs the message digest operation and appends a TSIG
    %%   record to the additional data section and transmits the request to
    %%   the server.  The client MUST store the message digest from the
    %%   request while awaiting an answer.
    %%
    true = ets:insert_new(?MODULE,
                          r(haystack_protocol:id(Pristine), RequestMAC)),

    {<<
       Packet2/binary,
       (rdata(TimeSigned, Fudge, ResponseMAC, OriginalID, Error,
              OtherData))/binary
     >>, Offsets2};


%% 4.1. TSIG generation on requests
%%
%%   Client performs the message digest operation and appends a TSIG
%%   record to the additional data section and transmits the request to
%%   the server.  The client MUST store the message digest from the
%%   request while awaiting an answer.  The digest components for a
%%   request are:
%%
%%      DNS Message (request)
%%      TSIG Variables (request)

encode(QR, #{algorithm := Algorithm,
             original_id := OriginalID,
             name := Name,
             fudge := Fudge,
             error := Error,
             other_data := OtherData},
       Pristine, WithHeader, Offsets1) when QR /= response ->

    TimeSigned = erlang:universaltime(),

    Digest = <<
               (reduce_arcount(Pristine))/binary,
               (variables(Name,
                          Algorithm,
                          TimeSigned,
                          Fudge,
                          Error,
                          OtherData))/binary
             >>,

    {ok, MAC} = hash(Algorithm, key(Name), Digest),

    {Packet2, Offsets2} = haystack_name:encode(Algorithm, WithHeader, Offsets1),
    {<<
       Packet2/binary,
       (rdata(TimeSigned, Fudge, MAC, OriginalID, Error, OtherData))/binary
     >>, Offsets2};

encode(QR, #{algorithm := _,
             original_id := _,
             name := _,
             error := _,
             other_data := _} = TSIG,
       Pristine, WithHeader, Offsets) when QR /= response ->
    encode(QR, TSIG#{fudge => haystack_config:tsig_rr_fudge()}, Pristine,
           WithHeader, Offsets).




rdata(MAC, ID, Error, OtherData) ->
    rdata(erlang:universaltime(), MAC, ID, Error, OtherData).

rdata(TimeSigned, MAC, ID, Error, OtherData) ->
    rdata(TimeSigned, haystack_config:tsig_rr_fudge(), MAC, ID, Error,
          OtherData).

rdata(TimeSigned, Fudge, MAC, OriginalID, Error, OtherData) ->
    <<
      (haystack_date:seconds_since_epoch(TimeSigned)):48,
      Fudge:16,
      (byte_size(MAC)):16,
      MAC/binary,
      OriginalID:16,
      (haystack_rcode:lookup(Error)):16,
      (byte_size(OtherData)):16,
      OtherData/binary
    >>.



%% Upon receipt of a message with a correctly placed TSIG RR, the
%% TSIG RR is copied to a safe location, removed from the DNS
%% Message, and decremented out of the DNS message header's
%% ARCOUNT. At this point the keyed message digest operation is
%% performed.
reduce_arcount(<<Prefix:80/bits, ARCOUNT:16, Postfix/binary>>) ->
    <<Prefix/binary, (ARCOUNT-1):16, Postfix/binary>>.



-define(ANY, 255).
-define(TTL, 0).

variables(Name, Algorithm, TimeSigned, Fudge, Error, OtherData) ->
    <<
      (haystack_name:encode(Name))/binary,
      ?ANY:16,
      ?TTL:32,
      (haystack_name:encode(Algorithm))/binary,
      (haystack_date:seconds_since_epoch(TimeSigned)):48,
      Fudge:16,
      (haystack_rcode:lookup(Error)):16,
      (byte_size(OtherData)):16,
      OtherData/bytes
    >>.

key(Name) ->
    case haystack_secret:find(Name) of
        not_found ->
            {error, bad_key};
        Secret ->
            Secret
    end.


hash(_, {error, _} = Error, _) ->
    Error;
hash([<<"hmac-md5">>, <<"sig-alg">>, <<"reg">>, <<"int">>], Key, Data) ->
    {ok, crypto:hmac(md5, Key, Data)}.


-record(?MODULE, {
           id :: integer(),
           mac :: binary()
          }).

r(Id, MAC) ->
    #?MODULE{id = Id, mac = MAC}.

mac(#?MODULE{mac = MAC}) ->
    MAC.
