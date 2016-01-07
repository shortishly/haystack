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
-export([getifaddrs/0]).
-export([getifaddrs/1]).

getifaddrs(v4) ->
    maps:fold(fun
                  (_, #{addr := {_, _, _, _} = IP}, A) ->
                      [IP | A];

                  (_, _, A) ->
                      A
              end,
              [],
              getifaddrs()).

getifaddrs() ->
    {ok, Interfaces} = inet:getifaddrs(),
    lists:foldl(fun
                    ({Interface, Properties}, A) ->
                        A#{Interface => maps:from_list(Properties)}
                end,
                #{},
                Interfaces).
