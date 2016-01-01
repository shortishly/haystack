%% Copyright (c) 2015-2016 Peter Morgan <peter.james.morgan@gmail.com>
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
-module(haystack_zone).
-export([parse/1]).
-export([strip/1]).

parse(B) when is_binary(B) ->
    parse(binary_to_list(B));
parse(S) ->
    {ok, Tokens, _} = haystack_zone_leexer:string(S),
    haystack_zone_grammar:parse(strip(Tokens)).


%% strip out single or multi-line parenthesis lines collapsing any
%% resource data into a single line

strip(Tokens) ->
    cleanup(outside(Tokens, [])).

cleanup(Tokens) ->
    cleanup(Tokens, []).

cleanup([{ws, _, _}, {eol, _} = H | T], A) ->
    cleanup([H | T], A);
cleanup([{ws, _, _}, {ws, _, _} = H | T], A) ->
    cleanup([H | T], A);
cleanup([{eol, _}, {eol, _} = H | T], A) ->
    cleanup([H | T], A);
cleanup([H | T], A) ->
    cleanup(T, [H | A]);
cleanup([], A) ->
    lists:reverse(A).

outside([{lparen, _} | T], A) ->
    inside(T, A);
outside([H | T], A) ->
    outside(T, [H | A]);
outside([], A) ->
    lists:reverse(A).

inside([{comment, _, _}, {eol, _} | T], A) ->
    inside(T, A);
inside([{rparen, _} | T], A) ->
    outside(T, A);
inside([{eol, _} | T], A) ->
    inside(T, A);
inside([H | T], A) ->
    inside(T, [H | A]).
