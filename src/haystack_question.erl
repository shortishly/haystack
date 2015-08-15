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

-module(haystack_question).
-export([decode/2]).
-export([encode/2]).

decode(Question, Packet) ->
    {Labels, <<
               QTYPE:16,
               QCLASS:16,
               Remainder/binary
             >>} = haystack_name:decode(Question, Packet),
    {#{type => haystack_rr:lookup(QTYPE),
       class => haystack_class:lookup(QCLASS),
       name => Labels}, Remainder}.


encode(Questions, Packet) ->
    encode(Questions, Packet, #{}).

encode([], Packet, Offsets) ->
    {Packet, Offsets};

encode([#{class := Class,
          name := Name,
          type := Type} | Questions], Packet1, Offsets1) ->
    {Packet2, Offsets2} = haystack_name:encode(Name, Packet1, Offsets1),
    encode(Questions, <<
                        Packet2/binary,
                        (haystack_rr:lookup(Type)):16,
                        (haystack_class:lookup(Class)):16
                      >>,
           Offsets2).
