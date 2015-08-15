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

-module(haystack_rr_mx).
-export([decode/3]).
-export([encode/4]).

decode(_, <<Preference:16, Exchange/binary>>, Packet) ->
    {Labels, <<>>} = haystack_name:decode(Exchange, Packet),
    #{preference => Preference, exchange => Labels}.

encode(#{exchange := Exchange, preference := Preference}, _, Packet, Offsets) ->
    haystack_name:encode(Exchange, <<Packet/binary, Preference:16>>, Offsets).
