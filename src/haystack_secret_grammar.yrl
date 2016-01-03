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

Header "%% Copyright (c) 2015 Peter Morgan <peter.james.morgan@gmail.com>".


Nonterminals

opening hash shared entries entry.

Terminals

string algorithm secret key lbrace rbrace semicolon variant.

Rootsymbol entries.

entries -> entry : ['$1'].

entries -> entry entries : [ '$1' | '$2'].

entry -> opening lbrace hash shared rbrace semicolon : #{
                                             key => '$1',
                                             algorithm => '$3',
                                             secret => '$4'
                                             }.

opening -> key string : binary:split(binary_value_of('$2'), <<".">>, [global]).

hash -> algorithm variant semicolon : value_of('$2').

shared -> secret string semicolon : base64:decode(value_of('$2')).


Erlang code.

binary_value_of(Symbol) ->
    list_to_binary(value_of(Symbol)).

value_of({Symbol, _}) ->
    Symbol;
value_of({_, _, Symbol}) ->
    Symbol.
