%% Copyright (c) 2012-2016 Peter Morgan <peter.james.morgan@gmail.com>
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

-module(haystack_sshd).
-behaviour(gen_server).

-export([start_link/0]).
-export([stop/0]).


-export([code_change/3]).
-export([handle_info/2]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([init/1]).
-export([terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

init([]) ->
    case ssh:daemon(22022, options()) of
        {ok, Daemon} ->
            {ok, #{daemon => Daemon}};

        {error, _} = Error ->
            {stop, Error, undefined}
    end.

options() ->
    [{inet, inet},
     {subsystems, []},
     {system_dir, filename:join(haystack:priv_dir(), "ssh")},
     {user_dir, filename:join(haystack:priv_dir(), "ssh")},
     {auth_methods, "publickey"}].

handle_call(stop, _, S) ->
    {stop, normal, ok, S}.

handle_cast(_, S) ->
    {stop, error, S}.

handle_info(_, S) ->
    {error, error, S}.

terminate(_, #{daemon := Daemon}) ->
    ssh:stop_daemon(Daemon).

code_change(_, State, _) ->
    {ok, State}.
