-module(dns_question).
-export([
	 decode/1
	]).

decode(Question) ->
    question(Question, []).

question(<<0:8, QTYPE:16, QCLASS:16, Remainder/binary>>, Labels) ->
    {#{type => QTYPE, class => QCLASS, name => lists:reverse(Labels)}, Remainder};
question(<<Length:8, Label:Length/bytes, Remainder/binary>>, Labels) ->
    question(Remainder, [Label | Labels]).

