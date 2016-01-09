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

-module(haystack_class).
-export([lookup/1]).

-on_load(on_load/0).

-record(?MODULE, {
           id :: pos_integer(),
           name :: atom(),
           description :: string()
          }).

on_load() ->
    haystack_table:new(?MODULE),
    add(1, in, <<"the internet">>),
    add(2, cs, <<"the CSNET class">>),
    add(3, ch, <<"the CHAOS class">>),
    add(4, hs, <<"Hesiod">>),
    add(254, none),
    add(255, any),
    ok.

add(Id, Name, Description) ->
    Record = #?MODULE{id = Id, name = Name, description = Description},
    ets:insert_new(?MODULE, Record) orelse
        error({badarg, [Id, Name, Description]}).

add(Id, Name) ->
    Record = #?MODULE{id = Id, name = Name},
    ets:insert_new(?MODULE, Record) orelse
        error({badarg, [Id, Name]}).

lookup(Id) when is_integer(Id) ->
    case ets:lookup(?MODULE, Id) of
        [#?MODULE{name = Name}] ->
            Name;

        [] ->
            error({badarg, Id})
    end;
lookup(Name) ->
    Pattern = #?MODULE{id = '_', name = Name, description = '_'},
    case ets:match_object(?MODULE, Pattern) of
        [#?MODULE{id = Id}] ->
            Id;

        [] ->
            error({badarg, Name})
    end.


