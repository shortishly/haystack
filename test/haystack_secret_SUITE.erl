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

-module(haystack_secret_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() ->
        [{group, samples}].

groups() ->
    [{samples, [parallel], common:all(?MODULE)}].

sample001_test(Config) ->
    gold(Config, "sample001").

gold(Config, Name) ->
    {ok, Parse} = parse(Config, Name ++ ".key"),
    ?assertEqual(common:consult(Config, Name ++ ".terms"), Parse).

parse(Config, Filename) ->
    L = binary_to_list(common:read_file(Config, Filename)),
    {ok, Tokens, _} = haystack_secret_leexer:string(L),
    haystack_secret_grammar:parse(Tokens).
