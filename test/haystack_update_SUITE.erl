%% Copyright (c) 2015 Peter Morgan <peter.james.morgan@gmail.com>
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

-module(haystack_update_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() ->
    [{group, samples}].

groups() ->
    [{samples, [parallel], common:all(?MODULE)}].


init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(haystack),
    Origin = [<<"example">>,<<"com">>],
    haystack_secret:add(Origin, base64:decode("l8DiN0kqlDhGVfD5wYwfDg==")),
    haystack_node:add(Origin,
                 in,
                 soa,
                 100,
                 #{m_name => [<<"ns">> | Origin],
                   r_name => [<<"hostmaster">> | Origin],
                   serial => 20,
                   refresh => 7200,
                   retry => 600,
                   expire => 3600000,
                   minimum => 60}),
    [{origin, Origin},
     {algorithm, [<<"hmac-md5">>,<<"sig-alg">>,<<"reg">>,<<"int">>]} | Config].

end_per_suite(_Config) ->
    common:purge_application(haystack).

example_com_soa_test(Config) ->
    Id = erlang:unique_integer(),
    Request = #{additional => [],
                answers => [],
                authority => [],
                header => #{aa => false,
                            id => Id,
                            opcode => query,
                            qr => query,
                            ra => false,
                            rcode => no_error,
                            rd => true,
                            tc => false,
                            z => 0},
                questions => [#{class => in,
                                name => origin(Config),
                                type => soa}]},

    ?assertMatch(#{answers := [#{data := #{m_name := [<<"ns">> | _],
                                           r_name := [<<"hostmaster">> | _]}}],
                   header := #{opcode := query,
                               qr := response,
                               rcode := no_error}}, process(Request)).

delete_oldhost_add_newhost_example_com_update_test(Config) ->
    Id = erlang:unique_integer(),
    Request = #{additional => [#{class => any,
                                 data => #{algorithm => algorithm(Config),
                                           error => no_error,
                                           name => origin(Config),
                                           original_id => Id,
                                           other_data => <<>>},
                                 labels => origin(Config),
                                 ttl => 0,
                                 type => tsig}],
                header => #{id => Id,
                            opcode => update,
                            qr => request,
                            rcode => no_error,
                            z => 0},
                prerequisites => [],
                updates => [#{class => any,
                              labels => [<<"oldhost">> | origin(Config)],
                              ttl => 0,
                              type => a},
                            #{class => in,
                              data => {172,16,1,1},
                              labels => [<<"newhost">> | origin(Config)],
                              ttl => 86400,
                              type => a}],
                zones => [#{class => in, name => origin(Config), type => soa}]},

    ?assertMatch(#{additional := [#{data := #{mac := _,
                                              time_signed := _,
                                              error := no_error},
                                    type := tsig}],
                   header := #{opcode := update,
                               qr := response,
                               rcode := no_error}}, process(Request)).


origin(Config) ->
    ?config(origin, Config).

algorithm(Config) ->
    ?config(algorithm, Config).

process(Packet) ->
    haystack_protocol:decode(haystack_protocol:process(haystack_protocol:encode(Packet))).
