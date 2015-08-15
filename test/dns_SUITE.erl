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

-module(dns_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() ->
        [{group, samples}].

groups() ->
    [{samples, [parallel], common:all(?MODULE)}].


init_per_suite(Config) ->
    application:ensure_all_started(pcapng),
    application:ensure_all_started(dns),
    Config.
    
dns_001_test(Config) ->
    gold(Config, "dns-001").
    
dns_002_test(Config) ->
    gold(Config, "dns-002").
    
dns_003_test(Config) ->
    gold(Config, "dns-003").
    
dns_004_test(Config) ->
    gold(Config, "dns-004").
    
dns_005_test(Config) ->
    gold(Config, "dns-005").
    
dns_006_test(Config) ->
    gold(Config, "dns-006").
    
dns_007_test(Config) ->
    gold(Config, "dns-007").
    
dns_008_test(Config) ->
    gold(Config, "dns-008").
    
dns_009_test(Config) ->
    gold(Config, "dns-009").
    
dns_010_test(Config) ->
    gold(Config, "dns-010").
    
dns_011_test(Config) ->
    gold(Config, "dns-011").
    
dns_012_test(Config) ->
    gold(Config, "dns-012").
    
dns_013_test(Config) ->
    gold(Config, "dns-013").
    
dns_014_test(Config) ->
    gold(Config, "dns-014").
    
dns_015_test(Config) ->
    gold(Config, "dns-015").
    
dns_016_test(Config) ->
    gold(Config, "dns-016").
    
dns_017_test(Config) ->
    gold(Config, "dns-017").

gold(Config, Name) ->
    ?assertEqual(common:consult(Config, Name ++ ".terms"), decode(Config, Name ++ ".pcapng")).

decode(Config, Filename) ->
    [dns_protocol:decode(Packet) || #{packet := #{ip := #{udp := #{data := Packet}}}, type := enhanced_packet_block} <- parse(Config, Filename)].

parse(Config, Name) ->
    pcapng:parse(common:read_file(Config, Name)).
