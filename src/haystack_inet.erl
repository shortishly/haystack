%% Copyright (c) 2016 Peter Morgan <peter.james.morgan@gmail.com>
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

-module(haystack_inet).
-export([getifaddrs/1]).

getifaddrs(v4) ->
    {ok, Interfaces} = inet:getifaddrs(),

    lists:filter(fun haystack_docker_util:is_same_network/1,
                 lists:foldl(fun
                                 ({_, Properties}, A) ->
                                     [Addr ||
                                         {_, _, _, _} = Addr <-
                                             proplists:get_all_values(
                                               addr, Properties),
                                         Addr /= {127, 0, 0, 1}] ++ A
                             end,
                             [],
                             Interfaces)).
