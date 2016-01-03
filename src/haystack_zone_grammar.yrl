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

nm header resource entries entry statement directive names domain rr
time ipv4.


Terminals

escape a ns cname soa mx origin dot ws integer duration name comment
control eol class.

Rootsymbol entries.


entries -> entry : [ '$1' ].
entries -> entry entries : [ '$1' | '$2' ].


entry -> statement eol : #{statement => '$1'}.
entry -> statement ws comment eol : #{statement => '$1',
                                      comment => binary_value_of('$3')}.
entry -> ws comment eol : #{comment => binary_value_of('$2')}.

statement -> directive : '$1'.
statement -> domain ws rr : #{domain => '$1', rr => '$3'}.
statement -> ws rr : #{rr => '$2'}.

directive -> control ws domain : #{control => value_of('$1'),
                                   domain => '$3'}.
directive -> control ws time : #{control => value_of('$1'),
                                 duration => '$3'}.

time -> duration : value_of('$1').
time -> integer : #{seconds => value_of('$1')}.

domain -> names : rel_or_abs('$1').
domain -> origin : value_of('$1').

names -> nm : ['$1'].
names -> nm dot : ['$1', value_of('$2')].
names -> nm dot names : ['$1', value_of('$2') | '$3'].

nm -> name : value_of('$1').
nm -> name escape name : value_of('$1') ++ value_of('$2') ++ value_of('$3').
nm -> a : "A".
nm -> class : value_of('$1').

rr -> header ws resource : maps:merge('$1', '$3').
rr -> resource : '$1'.

header -> class : #{class => as_class(value_of('$1'))}.

resource -> a ws ipv4 : #{type => a, rdata => '$3'}.

resource -> mx ws integer ws domain : #{type => mx,
                                        rdata => #{preference => value_of('$3'),
                                                   domain => '$5'}}.

resource -> ns ws domain : #{type => ns,
                             rdata => #{domain => '$3'}}.

resource -> cname ws domain : #{type => cname,
                                rdata => #{domain => '$3'}}.

resource -> soa ws domain ws domain ws integer ws time ws time ws time ws time : #{
                                                                            type => soa,
                                                                            rdata => #{
                                                                              m_name => '$3',
                                                                              r_name => '$5',
                                                                              serial => value_of('$7'),
                                                                              refresh => '$9',
                                                                              retry => '$11',
                                                                              expire => '$13',
                                                                              minimum => '$15'
                                                                              }}.


ipv4 -> integer dot integer dot integer dot integer : #{ip => {value_of('$1'),
                                                             value_of('$3'),
                                                             value_of('$5'),
                                                             value_of('$7')},
                                                      v => 4}.





Erlang code.

binary_value_of(Symbol) ->
    list_to_binary(value_of(Symbol)).

value_of({Symbol, _}) ->
    Symbol;
value_of({_, _, Symbol}) ->
    Symbol.

rel_or_abs(Names) ->
    rel_or_abs(Names, []).

rel_or_abs([], A) ->
    #{absolute => lists:reverse(A)};
rel_or_abs([Name, dot | Names], A) ->
    rel_or_abs(Names, [list_to_binary(Name) | A]);
rel_or_abs([Name], A) ->
    #{relative => lists:reverse([list_to_binary(Name) | A])}.

as_class("IN") ->
    in.
