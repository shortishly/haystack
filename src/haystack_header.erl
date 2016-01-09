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

-module(haystack_header).
-export([add/2]).
-export([add/3]).
-export([lookup/3]).

-on_load(on_load/0).

-record(?MODULE, {
           opcode :: atom(),
           field :: atom(),
           name :: atom(),
           value :: integer()
          }).

on_load() ->
    haystack_table:reuse(?MODULE, bag).

add(OPCODE, Field) ->
    add(OPCODE, Field, [false, true]).

add(OPCODE, Field, Names) ->
    [add(OPCODE, Field, Name, Value) ||
        {Name, Value} <-
            lists:zip(Names, lists:seq(0, length(Names)-1))],
    ok.

add(OPCODE, Field, Name, Value) ->
    true = ets:insert(?MODULE, [r(OPCODE, Field, Name, Value)]),
    ok.


lookup(OPCODE, Field, Name) when is_atom(Name) ->
    case ets:match_object(?MODULE, r(OPCODE, Field, Name, '_')) of
        [#?MODULE{value = Value}] ->
            Value;

        [] ->
            error({badarg, [OPCODE, Field, Name]})
    end;

lookup(OPCODE, Field, Value) when is_integer(Value) ->
    case ets:match_object(?MODULE, r(OPCODE, Field, '_', Value)) of
        [#?MODULE{name = Name}] ->
            Name;

        [] ->
            error({badarg, [OPCODE, Field, Value]})
    end.


r(OPCODE, Field, Name, Value) ->
    #?MODULE{opcode = OPCODE, field = Field, name = Name, value = Value}.
