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

-module(haystack_rr_srv).
-export([decode/3]).
-export([encode/4]).

decode(_, <<Priority:16, Weight:16, Port:16, Target/binary>>, Packet) ->
    {Labels, <<>>} = haystack_name:decode(Target, Packet),
    #{priority => Priority, weight => Weight, port => Port, target => Labels}.

encode(#{priority := Priority,
         weight := Weight,
         port := Port,
         target := Target},
       _,
       Packet,
       Offsets) ->
    haystack_name:encode(Target, <<
                                   Packet/binary,
                                   Priority:16,
                                   Weight:16,
                                   Port:16
                                 >>,
                         Offsets).
