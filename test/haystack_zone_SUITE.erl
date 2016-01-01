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

-module(haystack_zone_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() ->
        [{group, samples}].

groups() ->
    [{samples, [parallel], common:all(?MODULE)}].

simple_at_sign_test(_Config) ->
    {ok, [{origin, 1}], 1} = haystack_zone_leexer:string("@").

simple_comment_test(_Config) ->
    {ok, [{comment, 1, " abc"}], 1} = haystack_zone_leexer:string("; abc").

simple_escape_test(_Config) ->
    {ok, [{escape, 1, "."}],1} = haystack_zone_leexer:string("\\.").

simple_octet_test(_Config) ->
    {ok, [{octet, 1, 83}],1} = haystack_zone_leexer:string("\\123").

simple_left_paren_test(_Config) ->
    {ok, [{lparen, 1}],1} = haystack_zone_leexer:string("(").

simple_right_paren_test(_Config) ->
    {ok, [{rparen, 1}],1} = haystack_zone_leexer:string(")").

simple_integer_test(_Config) ->
    {ok, [{integer, 1, 666}],1} = haystack_zone_leexer:string("666").

simple_name_test(_Config) ->
    {ok, [{name, 1, "VENERA"}],1} = haystack_zone_leexer:string("VENERA").

simple_control_entity_origin_test(_Config) ->
    {ok,
     [{control, 1, origin}], 1} = haystack_zone_leexer:string("$ORIGIN").

simple_control_entity_include_test(_Config) ->
    {ok,
     [{control, 1, include}],1} = haystack_zone_leexer:string("$INCLUDE").

simple_control_entity_ttl_test(_Config) ->
    {ok,
     [{control, 1, ttl}],1} = haystack_zone_leexer:string("$TTL").


sample_001_test(Config) ->
    gold(Config, "sample001").

sample_002_test(Config) ->
    gold(Config, "sample002").

sample_003_test(Config) ->
    gold(Config, "sample003").

sample_004_test(Config) ->
    gold(Config, "sample004").

sample_005_test(Config) ->
    gold(Config, "sample005").

sample_006_test(Config) ->
    gold(Config, "sample006").

sample_007_test(Config) ->
    gold(Config, "sample007").

sample_008_test(Config) ->
    gold(Config, "sample008").

sample_009_test(Config) ->
    gold(Config, "sample009").

sample_010_test(Config) ->
    gold(Config, "sample010").

sample_011_test(Config) ->
    gold(Config, "sample011").

sample_012_test(Config) ->
    gold(Config, "sample012").

sample_013_test(Config) ->
    gold(Config, "sample013").

sample_014_test(Config) ->
    gold(Config, "sample014").

sample_015_test(Config) ->
    gold(Config, "sample015").

sample_016_test(_Config) ->
    %%    gold(Config, "sample016"),
    {skip, {not_implemented, aaaa}}.


sample_017_test(_Config) ->
    %%    gold(Config, "sample017").
    {skip, not_implemented}.

sample_018_test(Config) ->
    gold(Config, "sample018").

sample_019_test(Config) ->
    gold(Config, "sample019").

sample_020_test(Config) ->
    gold(Config, "sample020").

sample_021_test(Config) ->
    gold(Config, "sample021").

sample_022_test(Config) ->
    gold(Config, "sample022").

sample_023_test(Config) ->
    gold(Config, "sample023").

sample_024_test(Config) ->
    gold(Config, "sample024").

sample_025_test(Config) ->
    gold(Config, "sample025").

sample_026_test(Config) ->
    gold(Config, "sample026").

gold(Config, Name) ->
    {ok, Parse} = parse(Config, Name ++ ".zone"),
    ?assertEqual(common:consult(Config, Name ++ ".terms"), Parse).

parse(Config, Filename) ->
    haystack_zone:parse(common:read_file(Config, Filename)).
