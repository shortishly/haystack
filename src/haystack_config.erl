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
-export([enabled/1]).
-export([nameservers/0]).
-export([origin/0]).
-export([origin/1]).
-export([port/1]).
-export([tracing/0]).
-export([tsig_rr_fudge/0]).


port(udp) ->
    get_env(udp_port, 53);
port(http) ->
    get_env(http_port, 80);
port(http_alt) ->
    get_env(http_alt_port, 8080);
port(ssh) ->
    get_env(ssh_port, 22).

enabled(ssh) ->
    get_env(ssh_enabled, true).

get_env(Name, Default) ->
    haystack:get_env(Name, [app_env, {default, Default}]).

tracing() ->
    list_to_existing_atom(haystack:get_env(haystack_tracing,
                                           [os_env, {default, "false"}])).


acceptors(http) ->
    100;
acceptors(http_alt) ->
    100.

tsig_rr_fudge() ->
    300.

origin(services) ->
    <<"services.", (origin())/binary>>;

origin(dockers) ->
    <<"dockers.", (origin())/binary>>;

origin(containers) ->
    <<"containers.", (origin())/binary>>.

origin() ->
    list_to_binary(haystack:get_env(haystack_origin,
                                    [os_env, {default, "haystack"}])).

nameservers() ->
    lists:map(
      fun
          (Address) ->
              {ok, IP} = inet:parse_ipv4_address(Address),
              {IP, 53}
      end,
      string:tokens(
        haystack:get_env(
          haystack_nameservers,
          [os_env, {default, "8.8.8.8:8.8.4.4"}]),
        ":")).
