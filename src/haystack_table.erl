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

-module(haystack_table).
-export([new/1]).
-export([new/2]).
-export([reuse/1]).
-export([reuse/2]).

reuse(Name) ->
    reuse(Name, set).

reuse(Name, Type) ->
    reuse(Name, Type, options()).

reuse(Name, Type, Options) ->
    try
        Name = ets:new(Name, [Type | Options]),
        ok
    catch
        error:badarg ->
            ok
    end.

new(Name) ->
    new(Name, set).

new(Name, Type) ->
    new(Name, Type, options()).

new(Name, Type, Options) ->
    try
        Name = ets:new(Name, [Type | Options])
    catch
        error:badarg ->
            true = ets:delete_all_objects(Name)
    end,
    ok.

options() ->
    [named_table, public, {keypos, 2}, haystack_table_owner:heir()].
