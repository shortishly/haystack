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

-module(haystack_table_owner).
-behaviour(gen_server).

%% API.
-export([heir/0]).
-export([start_link/0]).

%% gen_server.
-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([terminate/2]).

%% API.

-ifdef(NOTEST).
heir() ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) ->
            {heir, Pid, []};

        undefined ->
            error(badarg)
    end.
-else.
heir() ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) ->
            {heir, Pid, []};

        undefined ->
            {ok, Pid} = gen_server:start({local, ?MODULE}, ?MODULE, [], []),
            {heir, Pid, []}
    end.
-endif.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% gen_server

init([]) ->
    {ok, #{tables => []}}.


handle_call(_, _, State) ->
    {stop, error, State}.

handle_cast(_, State) ->
    {stop, error, State}.

handle_info({'ETS-TRANSFER', Table, _, _}, #{tables := Tables} = State) ->
    {noreply, State#{tables := [Table | Tables]}}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.
