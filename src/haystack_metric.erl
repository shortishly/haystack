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

-module(haystack_metric).
-export([all/0]).
-export([increment/1]).
-export([value/1]).

-on_load(on_load/0).

-record(?MODULE, {
           key,
           value
          }).

on_load() ->
    crown_table:reuse(?MODULE, set).

all() ->
    lists:foldl(
      fun
          (#?MODULE{key = Key, value = Value}, A) ->
              A#{Key => Value}
      end,
      #{},
      ets:tab2list(?MODULE)).


increment(Key) ->
    ets:update_counter(?MODULE, Key, 1, #?MODULE{value = 0}).

value(Key) ->
    case ets:lookup(?MODULE, Key) of
        [#?MODULE{value = Value}] ->
            Value;

        [] ->
            0
    end.
    
