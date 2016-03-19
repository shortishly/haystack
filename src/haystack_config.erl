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

-module(haystack_config).

-export([acceptors/1]).
-export([origin/0]).
-export([origin/1]).
-export([port/1]).
-export([tracing/0]).
-export([tsig_rr_fudge/0]).


port(udp) ->
    envy:to_integer(haystack, udp_port, default(53));
port(http) ->
    envy:to_integer(haystack, http_port, default(80));
port(http_alt) ->
    envy:to_integer(haystack, http_alt_port, default(8080)).

tracing() ->
    envy:to_boolean(haystack, tracing, default(false)).


acceptors(http) ->
    envy:to_integer(haystack, http_acceptors, default(100));
acceptors(http_alt) ->
    envy:to_integer(haystack, http_alt_acceptors, default(100)).

tsig_rr_fudge() ->
    envy:to_integer(haystack, tsig_rr_fudge, default(300)).


origin() ->
    envy:to_binary(haystack, origin, default(<<"haystack">>)).

origin(ns) ->
    <<"ns.", (origin())/binary>>;

origin(services) ->
    <<"services.", (origin())/binary>>;

origin(dockers) ->
    <<"dockers.", (origin())/binary>>;

origin(containers) ->
    <<"containers.", (origin())/binary>>.


default(Default) ->
    [os_env, app_env, {default, Default}].
