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

-module(haystack_rr_tsig_SUITE).
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
    [{origin, Origin},
     {algorithm, [<<"hmac-md5">>,<<"sig-alg">>,<<"reg">>,<<"int">>]} | Config].

end_per_suite(_Config) ->
    common:purge_application(haystack).

%% simple decode of an encoded request
decode_encode_request_test(Config) ->
    Id = 38836,
    Packet = #{additional => [#{class => any,
                                data => #{algorithm => algorithm(Config),
                                          error => no_error,
                                          name => origin(Config),
                                          original_id => Id,
                                          other_data => <<>>},
                                labels => origin(Config),
                                ttl => 0,
                                type => tsig}],
               answers => [],
               authority => [],
               header => #{aa => false,
                           id => Id,
                           opcode => query,
                           qr => query,
                           ra => false,
                           rcode => no_error,
                           rd => false,
                           tc => false,
                           z => 0},
               questions => [#{class => in,
                               name => [<<"oldhost">> | origin(Config)],
                               type => soa}]},
    #{additional := [#{type := tsig,
                       data := #{mac := _,
                                 time_signed := _,
                                 error := no_error}}]} = decode_encode(Packet).

decode_after_fudge_elased_encode_request_test(Config) ->
    Id = 38837,
    Packet = #{additional => [#{class => any,
                                data => #{algorithm => algorithm(Config),
                                          error => no_error,
                                          name => origin(Config),
                                          fudge => 1,
                                          original_id => Id,
                                          other_data => <<>>},
                                labels => origin(Config),
                                ttl => 0,
                                type => tsig}],
               answers => [],
               authority => [],
               header => #{aa => false,
                           id => Id,
                           opcode => query,
                           qr => query,
                           ra => false,
                           rcode => no_error,
                           rd => false,
                           tc => false,
                           z => 0},
               questions => [#{class => in,
                               name => [<<"oldhost">> | origin(Config)],
                               type => soa}]},

    Encoded = haystack_protocol:encode(Packet),
    timer:sleep(2000),
    #{additional := [#{type := tsig,
                       data := Data}]} = haystack_protocol:decode(Encoded),
    ?assertMatch(#{mac := _,
                   time_signed := _,
                   error := bad_time}, Data).


%% simple decode of an encoded response
decode_encode_response_test(Config) ->
    MAC = <<84,3,53,146,5,9,228,202,38,150,131,71,35,145,1,23>>,
    Packet = #{additional => [#{class => any,
                                data => #{algorithm => algorithm(Config),
                                          error => no_error,
                                          fudge => 300,
                                          mac => MAC,
                                          name => origin(Config),
                                          original_id => 38836,
                                          other_data => <<>>},
                                labels => origin(Config),
                                ttl => 0,
                                type => tsig}],
               answers => [],
               authority => [],
               header => #{aa => false,
                           id => 38836,
                           opcode => query,
                           qr => response,
                           ra => false,
                           rcode => name_error,
                           rd => false,
                           tc => false,
                           z => 0},
               questions => [#{class => in,
                               name => [<<"oldhost">> | origin(Config)],
                               type => soa}]},
    decode_encode(Packet).

origin(Config) ->
    ?config(origin, Config).

algorithm(Config) ->
    ?config(algorithm, Config).

decode_encode(Packet) ->
    haystack_protocol:decode(haystack_protocol:encode(Packet)).
