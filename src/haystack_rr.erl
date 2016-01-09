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

-module(haystack_rr).
-export([decode/2]).
-export([encode/3]).
-export([lookup/1]).
-export([questions/3]).
-export([resources/3]).
-export([zones/3]).

-on_load(on_load/0).

-define(OPT, 41).

-record(?MODULE, {
           id :: pos_integer(),
           name :: atom(),
           coder :: atom()
          }).

on_load() ->
    haystack_table:new(?MODULE),
    add(1, a, haystack_rr_a),
    add(2, ns, haystack_rr_ns),
    add(5, cname, haystack_rr_cname),
    add(6, soa, haystack_rr_soa),
    add(12, ptr, haystack_rr_ptr),
    add(15, mx, haystack_rr_mx),
    add(16, txt, haystack_rr_txt),
    add(28, aaaa, haystack_rr_aaaa),
    add(33, srv, haystack_rr_srv),
    add(41, opt, haystack_rr_opt),
    add(250, tsig, haystack_rr_tsig),
    add(255, any).

add(Id, Name, Coder) ->
    ets:insert_new(?MODULE, r(Id, Name, Coder)) orelse
        error({badarg, [Id, Name, Coder]}),
    ok.

add(Id, Name) ->
    ets:insert_new(?MODULE, r(Id, Name)) orelse
        error({badarg, [Id, Name]}),
    ok.


lookup(Id) when is_integer(Id) ->
    case ets:lookup(?MODULE, Id) of
        [#?MODULE{name = Name}] ->
            Name;

        [] ->
            error({badarg, Id})
    end;
lookup(Name) ->
    case ets:match_object(?MODULE, r('_', Name, '_')) of
        [#?MODULE{id = Id}] ->
            Id;

        [] ->
            error({badarg, Name})
    end.


coder(Id) when is_integer(Id) ->
    case ets:lookup(?MODULE, Id) of
        [#?MODULE{coder = Coder}] ->
            Coder;

        [] ->
            error({badarg, Id})
    end;
coder(Name) ->
    case ets:match_object(?MODULE, r('_', Name, '_')) of
        [#?MODULE{coder = Coder}] ->
            Coder;

        [] ->
            error({badarg, Name})
    end.



encode(Packet, Offsets, []) ->
    {Packet, Offsets};


encode(Packet1, Offsets1, [#{do := DO,
                             ext_rcode := ExtendedRCode,
                             type := Type,
                             udp_payload := UDPPayload,
                             version := Version} | Resources]) ->
    {Packet2, Offsets2} = haystack_name:encode([], Packet1, Offsets1),
    encode(<<
             Packet2/binary,
             (lookup(Type)):16,
             UDPPayload:16,
             ExtendedRCode:8,
             Version:8,
             DO:1,
             0:15,
             0:16
           >>,
           Offsets2,
           Resources);

encode(Packet1, Offsets1, [#{type := Type,
                             class := Class,
                             data := Data,
                             ttl := TTL,
                             labels := Labels} | Resources]) ->
    {Packet2, Offsets2} = haystack_name:encode(Labels, Packet1, Offsets1),
    {Packet3, Offsets3} = encode_data(Packet1,
                                      <<
                                        Packet2/binary,
                                        (lookup(Type)):16,
                                        (haystack_class:lookup(Class)):16,
                                        TTL:32,
                                        0:16
                                      >>,
                                      Offsets2,
                                      coder(Type),
                                      Data),
    encode(Packet3, Offsets3, Resources);

encode(Packet1, Offsets1, [#{type := Type,
                             class := Class,
                             ttl := TTL,
                             labels := Labels} | Resources]) ->
    {Packet2, Offsets2} = haystack_name:encode(Labels, Packet1, Offsets1),
    encode(<<
             Packet2/binary,
             (lookup(Type)):16,
             (haystack_class:lookup(Class)):16,
             TTL:32,
             0:16>>,
           Offsets2,
           Resources).


encode_data(Pristine, PacketWithHeader, Offsets1, Encoder, Data) ->
    WithoutLength = byte_size(PacketWithHeader)-2,
    {<<
       Packet2:WithoutLength/bytes,
       0:16,
       Encoded/binary>>,
     Offsets2} = apply(Encoder, encode, [Data,
                                         Pristine, PacketWithHeader, Offsets1]),
    {<<
       Packet2/binary,
       (byte_size(Encoded)):16,
       Encoded/binary
     >>,
     Offsets2}.


decode(<<>>, _) ->
    {#{}, <<>>};

decode(Resource, <<_:16, _:12, _RCODE:4, _/binary>> = Packet) ->
    case haystack_name:decode(Resource, Packet) of
        %% https://tools.ietf.org/html/rfc6891#section-6.1.2
        {[], <<
               Type:16,
               UDPPayload:16,
               ExtendedRCode:8,
               Version:8,
               DO:1,
               _:15,
               0:16,
               Remainder/binary
             >>} when Type == ?OPT ->
            {#{type => lookup(Type),
               udp_payload => UDPPayload,
               ext_rcode => ExtendedRCode,
               version => Version,
               do => DO}, Remainder};

        %% https://tools.ietf.org/html/rfc6891#section-6.1.2
        {[], <<
               Type:16,
               UDPPayload:16,
               ExRCODE:8,
               Version:8,
               DO:1,
               _:15,
               Length:16,
               Data:Length/bytes,
               Remainder/binary
             >>} when Type == ?OPT ->
            {#{type => lookup(Type),
               udp_payload => UDPPayload,
               ext_rcode => ExRCODE,
               version => Version,
               do => DO,
               data => (coder(Type)):decode(Resource, Data, Packet)},
             Remainder};

        {Labels, <<Type:16, Class:16, TTL:32, 0:16, Remainder/binary>>} ->
            {#{type => lookup(Type),
               class => haystack_class:lookup(Class),
               ttl => TTL,
               labels => Labels}, Remainder};

        {Labels, <<
                   Type:16,
                   Class:16,
                   TTL:32,
                   Length:16,
                   Data:Length/bytes,
                   Remainder/binary
                 >>} ->
            {#{type => lookup(Type),
               class => haystack_class:lookup(Class),
               ttl => TTL,
               data => (coder(Type)):decode(Resource, Data, Packet),
               labels => Labels}, Remainder}
    end.

zones(Count, Zones, Packet) ->
    unmarshall(Count, Zones, Packet, haystack_question, []).

questions(Count, Questions, Packet) ->
    unmarshall(Count, Questions, Packet, haystack_question, []).

resources(Count, Resources, Packet) ->
    unmarshall(Count, Resources, Packet, haystack_rr, []).

unmarshall(0, Remainder, _, _, A) ->
    {lists:reverse(A), Remainder};
unmarshall(N, Resources, Packet, Decoder, A) ->
    {Resource, Remainder} = apply(Decoder, decode, [Resources, Packet]),
    unmarshall(N-1, Remainder, Packet, Decoder, [Resource | A]).

r(Id, Name) ->
    #?MODULE{id = Id, name = Name}.

r(Id, Name, Coder) ->
    #?MODULE{id = Id, name = Name, coder = Coder}.
