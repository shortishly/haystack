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

-module(haystack_query).
-export([add_header/1]).
-export([add_header/2]).
-export([decode/1]).
-export([encode/1]).
-export([header/2]).
-export([process/1]).

-on_load(on_load/0).

on_load() ->
    add_header(qr, [query, response]),
    add_header(aa),
    add_header(tc),
    add_header(rd),
    add_header(ra).

add_header(Name) ->
    haystack_header:add(?MODULE, Name).

add_header(Name, Mapping) ->
    haystack_header:add(?MODULE, Name, Mapping).


process(#{header := Header,
          additional := [#{type := tsig, error := bad_key} = TSIG]} = Packet) ->
    Packet#{header => Header#{qr => response, rcode => not_auth},
            answers => [],
            authority => [],
            additional => [TSIG]};

process(#{header := Header, questions := Questions} = Packet) ->
    lists:foldl(
      fun
          (#{name := Name, class := Class, type := Type}, A) ->
              case haystack_res:lookup(Name, Class, Type) of
                  not_found ->
                      A#{header => Header#{qr => response,
                                           rcode => name_error}};

                  Resources ->
                      A#{header => Header#{qr => response,
                                           rcode => no_error},
                         answers => Resources}
              end
      end,
      Packet,
      Questions);

process(Packet) ->
    encode(process(decode(Packet))).


encode(#{header := #{id := ID,
                     qr := QR,
                     opcode := OPCODE,
                     aa := AA,
                     tc := TC,
                     rd := RD,
                     ra := RA,
                     z := Z,
                     rcode := RCODE},
         questions := Questions,
         answers := Answers,
         authority := Authorities,
         additional := Additionals}) ->

    Header = <<
               ID:16,
               (header(qr, QR)):1,
               (haystack_protocol:lookup(OPCODE)):4,
               (header(aa, AA)):1,
               (header(tc, TC)):1,
               (header(rd, RD)):1,
               (header(ra, RA)):1,
               Z:3,
               (haystack_rcode:lookup(RCODE)):4,
               (length(Questions)):16,
               (length(Answers)):16,
               (length(Authorities)):16,
               (length(Additionals)):16
             >>,

    {HeaderAndQuestions, Offsets} = haystack_question:encode(Questions, Header),
    {Packet, _} = haystack_rr:encode(HeaderAndQuestions,
                                     Offsets,
                                     Answers ++ Authorities ++ Additionals),
    Packet.

decode(<<
         ID:16,
         QR:1, OPCODE:4, AA:1, TC:1, RD:1, RA:1, Z:3, RCODE:4,
         QDCOUNT:16,
         ANCOUNT:16,
         NSCOUNT:16,
         ARCOUNT:16,
         Remainder/binary
       >> = Packet) ->

    Header = #{id => ID,
               qr => header(qr, QR),
               opcode => haystack_protocol:lookup(OPCODE),
               aa => header(aa, AA),
               tc => header(tc, TC),
               rd => header(rd, RD),
               ra => header(ra, RA),
               z => Z,
               rcode => haystack_rcode:lookup(RCODE),
               qd_count => QDCOUNT,
               an_count => ANCOUNT,
               ns_count => NSCOUNT,
               ar_count => ARCOUNT
              },

    {Questions, AnswerRemainder} = haystack_rr:questions(QDCOUNT, Remainder,
                                                         Packet),

    {Answers, AuthorityRemainder} = haystack_rr:resources(ANCOUNT,
                                                          AnswerRemainder,
                                                          Packet),
    {Authority, AdditionalRemainder} = haystack_rr:resources(NSCOUNT,
                                                             AuthorityRemainder,
                                                             Packet),
    {Additional, <<>>} = haystack_rr:resources(ARCOUNT, AdditionalRemainder,
                                               Packet),

    #{header => Header,
      questions => Questions,
      answers => Answers,
      authority => Authority,
      additional => Additional}.

header(Field, Value) ->
    haystack_header:lookup(?MODULE, Field, Value).
