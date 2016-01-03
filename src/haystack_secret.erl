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

-module(haystack_secret).

-export([add/2]).
-export([find/1]).
-export([process/1]).
-export([remove/1]).

-on_load(on_load/0).

on_load() ->
    haystack_table:new(?MODULE),
    ok.

-record(?MODULE, {
           name,
           secret
          }).

add(Name, Secret) ->
    ets:insert_new(?MODULE, [r(Name, Secret)]) orelse
        error({badarg, [Name, Secret]}).

find(Name) ->
    case ets:lookup(?MODULE, Name) of
        [] ->
            not_found;

        [#?MODULE{secret = Secret}] ->
            Secret
    end.

remove(_Name) ->
    ok.

r(Name, Secret) ->
    #?MODULE{name = Name, secret = Secret}.


process(Secrets) ->
    case parse(Secrets) of
        {ok, Keys} ->
            lists:foreach(fun key/1, Keys),
            ok;
        {error, {Line, Module, Message}} ->
            {error, #{line => Line, module => Module, message => Message}}
    end.


parse(Secrets) when is_binary(Secrets) ->
    parse(binary_to_list(Secrets));
parse(Secrets) ->
    {ok, Tokens, _} = haystack_secret_leexer:string(Secrets),
    haystack_secret_grammar:parse(Tokens).


key(#{algorithm := "hmac-md5", key := Name, secret := Secret}) ->
    add(Name, Secret).
