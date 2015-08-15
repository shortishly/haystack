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
-module(haystack_name).
-export([decode/2]).
-export([encode/1]).
-export([encode/3]).


%% https://tools.ietf.org/html/rfc1035, section: 3.1.
%% Domain names in messages are expressed in terms of a sequence of
%% labels.  Each label is represented as a one octet length field
%% followed by that number of octets.  Since every domain name ends
%% with the null label of the root, a domain name is terminated by a
%% length byte of zero.  The high order two bits of every length octet
%% must be zero, and the remaining six bits of the length field limit
%% the label to 63 octets or less.

decode(Data, Packet) ->
    {Labels, Remainder} = decode(Data, Packet, []),
    {lists:reverse(Labels), Remainder}.

decode(<<1:1, 1:1, Offset:14, Remainder/binary>>, Packet, A) ->
    <<_:Offset/bytes, Pointer/binary>> = Packet,
    {Labels, _} = decode(Pointer, Packet, A),
    {Labels, Remainder};
decode(<<0:8, Remainder/binary>>, _, A) ->
    {A, Remainder};
decode(<<0:2, Length:6, Label:Length/bytes, Remainder/binary>>, Packet, A) ->
    decode(Remainder, Packet, [Label | A]).


encode(Labels) ->
    encode(Labels, <<>>).

encode([], A) ->
    <<A/binary, 0:8>>;
encode([Label | Labels], A) when byte_size(Label) =< 63 ->
    encode(Labels, <<A/binary, 0:2, (byte_size(Label)):6, Label/bytes>>).


encode([], Packet, Offsets) ->
    {<<Packet/binary, 0:8>>, Offsets};

encode([Label | T] = Labels, Packet, Offsets) when byte_size(Label) =< 63 ->
    case maps:find(Labels, Offsets) of
        {ok, Offset} ->
            {<<Packet/binary, 1:1, 1:1, Offset:14>>, Offsets};

        error ->
            encode(T, <<
                        Packet/binary,
                        0:2,
                        (byte_size(Label)):6,
                        Label/bytes
                      >>,
                   Offsets#{Labels => byte_size(Packet)})
    end.


