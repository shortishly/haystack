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

-module(haystack_sup).
-behaviour(supervisor).

-export([init/1]).
-export([start_link/0]).
-export([supervisor/1]).
-export([supervisor/2]).
-export([supervisor/3]).
-export([supervisor/4]).
-export([worker/1]).
-export([worker/2]).
-export([worker/3]).
-export([worker/4]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [supervisor(haystack_docker_sup),
             worker(haystack_sshd)],
    {ok, {#{intensity => 5, period => 5}, Procs}}.

supervisor(Module) ->
    supervisor(Module, permanent).

supervisor(Module, Restart) ->
    supervisor(Module, Restart, []).

supervisor(Module, Restart, Parameters) ->
    supervisor(Module, Module, Restart, Parameters).

supervisor(Id, Module, Restart, Parameters) ->
    #{id => Id,
      start => {Module, start_link, Parameters},
      restart => Restart,
      type => supervisor,
      shutdown => infinity}.


worker(Module) ->
    worker(Module, permanent).

worker(Module, Restart) ->
    worker(Module, Restart, []).

worker(Module, Restart, Parameters) ->
    worker(Module, Module, Restart, Parameters).

worker(Id, Module, Restart, Parameters) ->
    #{id => Id,
      start => {Module, start_link, Parameters},
      restart => Restart,
      shutdown => 5000}.
