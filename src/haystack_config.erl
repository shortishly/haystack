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
-export([debug/1]).
-export([docker/1]).
-export([enabled/1]).
-export([origin/0]).
-export([origin/1]).
-export([port/1]).
-export([tsig_rr_fudge/0]).


%% We are not using envy for the following docker environment variable
%% lookups because we don't want to prefix the environment variable
%% with the haystack application name.
docker(host) ->
    haystack:get_env(docker_host, [os_env]);
docker(cert_path) ->
    haystack:get_env(docker_cert_path, [os_env]);
docker(cert) ->
    haystack:get_env(docker_cert, [os_env]);
docker(key) ->
    haystack:get_env(docker_key, [os_env]).



port(udp) ->
    envy(to_integer, udp_port, 53);
port(http) ->
    envy(to_integer, http_port, 80);
port(http_alt) ->
    envy(to_integer, http_alt_port, 8080).

enabled(debug) ->
    envy(to_boolean, debug, false).

debug(applications) ->
    envy(to_list, debug_applications, "haystack").

acceptors(http) ->
    envy(to_integer, http_acceptors, 100);
acceptors(http_alt) ->
    envy(to_integer, http_alt_acceptors, 100).

tsig_rr_fudge() ->
    envy(to_integer, tsig_rr_fudge, 300).


origin() ->
    envy(to_binary, origin, <<"haystack">>).

origin(ns) ->
    <<"ns.", (origin())/binary>>;

origin(services) ->
    <<"services.", (origin())/binary>>;

origin(dockers) ->
    <<"dockers.", (origin())/binary>>;

origin(containers) ->
    <<"containers.", (origin())/binary>>.

envy(To, Name, Default) ->
    envy:To(haystack, Name, default(Default)).

default(Default) ->
    [os_env, app_env, {default, Default}].
