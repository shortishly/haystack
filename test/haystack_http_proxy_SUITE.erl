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

-module(haystack_http_proxy_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() ->
        [{group, samples}].

groups() ->
    [{samples, [parallel], common:all(?MODULE)}].

init_per_suite(C0) ->
    {ok, _} = application:ensure_all_started(inets),
    Port = 8888,
    C1 = [{http_port, Port} | C0],
    {ok, _} = cowboy:start_http(?MODULE,
                                10,
                                [{port, Port}],
                                [{env, [dispatch(C1)]}]),
    C1.

end_per_suite(_Config) ->
    cowboy:stop_listener(?MODULE).

http_port(Config) ->
    ?config(http_port, Config).

dispatch(Config) ->
    {dispatch, cowboy_router:compile(resources(Config))}.

resources(Config) ->
    [{'_',
      [{<<"/sample/simple">>, proxy_simple_resource, []},
       {<<"/proxy">>,
        haystack_http_proxy_resource,
        #{prefix => <<>>,
          balancer => balancer(Config)}}]}].

balancer(Config) ->
    fun(_) -> #{host => "127.0.0.1", port => http_port(Config)} end.

simple_test(Config) ->
    httpc:request("http://localhost: " ++
                      integer_to_list(http_port(Config)) ++
                      "/proxy/sample/simple").
