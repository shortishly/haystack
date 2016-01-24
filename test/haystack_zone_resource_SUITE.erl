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

-module(haystack_zone_resource_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() ->
    [{group, samples}].

groups() ->
    [{samples, [sequence], common:all(?MODULE)}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = cowboy:start_http(?MODULE,
                                       10,
                                       [{port, 0}],
                                       [{env, [dispatch()]}]),
    Config.

end_per_suite(_Config) ->
    cowboy:stop_listener(?MODULE),
    application:stop(inets),
    application:stop(cowboy).

data_dir(Config) ->
    ?config(data_dir, Config).

http_port() ->
    ranch_server:get_port(?MODULE).

dispatch() ->
    {dispatch, cowboy_router:compile(resources())}.

resources() ->
    [{<<"localhost">>,
      [{<<"/zones">>, haystack_zone_resource, []}]}].

sample(Config, Filename) ->
    filename:join(data_dir(Config),
                  Filename).

sample_019_test(Config) ->
    {ok, Records} = file:read_file(sample(Config, "sample019.zone")),
    {ok,
     {{_, 204, _},
      _,
      _}} = request(post, "/zones", Records).


request(post, URL, Body) ->
    httpc:request(post,
                  {"http://localhost:" ++
                       integer_to_list(http_port()) ++
                       URL,
                   [],
                  "text/dns",
                  Body},
                  [],
                  [{body_format, binary}]).
