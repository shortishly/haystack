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

-module(haystack_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() ->
        [{group, samples}].

groups() ->
    [{samples, [parallel], common:all(?MODULE)}].


init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(pcapng),
    {ok, _} = application:ensure_all_started(haystack),
    Config.

end_per_suite(_Config) ->
    common:purge_application(haystack),
    common:purge_application(pcapng).

dns_001_test(Config) ->
    gold(Config, "dns-001").

reencode_001_test(Config) ->
    reencode(Config, "dns-001").

dns_002_test(Config) ->
    gold(Config, "dns-002").

reencode_002_test(Config) ->
    reencode(Config, "dns-002").

dns_003_test(Config) ->
    gold(Config, "dns-003").

reencode_003_test(Config) ->
    reencode(Config, "dns-003").

dns_004_test(Config) ->
    gold(Config, "dns-004").

reencode_004_test(Config) ->
    reencode(Config, "dns-004").

dns_005_test(Config) ->
    gold(Config, "dns-005").

reencode_005_test(Config) ->
    reencode(Config, "dns-005").

dns_006_test(Config) ->
    gold(Config, "dns-006").

reencode_006_test(Config) ->
    reencode(Config, "dns-006").

dns_007_test(Config) ->
    gold(Config, "dns-007").

reencode_007_test(Config) ->
    reencode(Config, "dns-007").

dns_008_test(Config) ->
    gold(Config, "dns-008").

reencode_008_test(Config) ->
    reencode(Config, "dns-008").

dns_009_test(Config) ->
    gold(Config, "dns-009").

reencode_009_test(_Config) ->
    %%    reencode(Config, "dns-009").
    {skip, truncate_not_implemented}.

dns_010_test(Config) ->
    gold(Config, "dns-010").

reencode_010_test(_Config) ->
    %%    reencode(Config, "dns-010"),
    {skip, truncate_not_implemented}.

dns_011_test(Config) ->
    gold(Config, "dns-011").

reencode_011_test(Config) ->
    reencode(Config, "dns-011").

dns_012_test(Config) ->
    gold(Config, "dns-012").

reencode_012_test(Config) ->
    reencode(Config, "dns-012").

dns_013_test(Config) ->
    gold(Config, "dns-013").

reencode_013_test(Config) ->
    reencode(Config, "dns-013").

dns_014_test(Config) ->
    gold(Config, "dns-014").

reencode_014_test(Config) ->
    reencode(Config, "dns-014").

dns_015_test(Config) ->
    gold(Config, "dns-015").

reencode_015_test(Config) ->
    reencode(Config, "dns-015").

dns_016_test(Config) ->
    gold(Config, "dns-016").

reencode_016_test(Config) ->
    reencode(Config, "dns-016").

dns_017_test(Config) ->
    gold(Config, "dns-017").

reencode_017_test(Config) ->
    reencode(Config, "dns-017").

%%dns_018_test(Config) ->
%%    gold(Config, "dns-018").

dns_019_test(_Config) ->
    %%    gold(Config, "dns-019").
    {skip, invalid_test_response_without_request}.

reencode_019_test(_Config) ->
%%    reencode(Config, "dns-019").
    {skip, invalid_test}.

gold(Config, Name) ->
    [Term] = common:consult(Config, Name ++ ".terms"),
    ?assertEqual(Term,
                 decode(Config, Name ++ ".pcapng")).

decode(Config, Filename) ->
    [haystack_protocol:decode(Packet) ||
        #{packet := #{ip := #{udp := #{data := Packet}}},
          type := enhanced_packet_block} <- parse(Config, Filename)].

packets(Config, Filename) ->
    [Packet || #{packet := #{ip := #{udp := #{data := Packet}}},
                 type := enhanced_packet_block} <- parse(Config, Filename)].

parse(Config, Name) ->
    pcapng:parse(common:read_file(Config, Name)).


reencode(Config, Name) ->
        lists:foreach(fun
                          (Packet) ->
                              Decoded = haystack_protocol:decode(Packet),
                              ct:log("decoded: ~p", [Decoded]),
                              Encoded = haystack_protocol:encode(Decoded),
                              Deredecoded = haystack_protocol:decode(Encoded),
                              ct:log("deredecoded: ~p", [Deredecoded]),
                              ?assertEqual(Decoded, Deredecoded)
%%                            ?assertEqual(Packet, haystack_protocol:encode(Decoded))
                      end,
                      packets(Config, Name ++ ".pcapng")).
