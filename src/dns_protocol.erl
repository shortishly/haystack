-module(dns_protocol).
-export([
	 decode/1
	]).

decode(<<_ID:16, _QR:1, _OPCODE:4, _AA:1, _TC:1, _RD:1, _RA:1, _Z:3, _RCODE:4, QDCOUNT:16, ANCOUNT:16, NSCOUNT:16, ARCOUNT:16, Remainder/binary>> = Packet) ->
    {Questions, AnswerRemainder} = questions(QDCOUNT, Remainder),
    {Answers, AuthorityRemainder} = resources(ANCOUNT, AnswerRemainder, Packet),
    {Authority, AdditionalRemainder} = resources(NSCOUNT, AuthorityRemainder, Packet),     
    {Additional, <<>>} = resources(ARCOUNT, AdditionalRemainder, Packet),
    #{header => dns_header:decode(Packet), questions => Questions, answers => Answers, authority => Authority, additional => Additional}.


questions(QDCOUNT, Questions) ->
    questions(QDCOUNT, Questions, []).     

questions(0, Remainder, A) ->
    {A, Remainder};
questions(N, Questions, A) ->
    {Question, Remainder} = dns_question:decode(Questions),
    questions(N-1, Remainder, [Question | A]).

resources(Count, Resources, Packet) ->
    resources(Count, Resources, Packet, []).

resources(0, Remainder, _, A) ->
    {A, Remainder};
resources(N, Resources, Packet, A) ->
    {Resource, Remainder} = dns_rr:decode(Resources, Packet),
    resources(N-1, Remainder, Packet, [Resource | A]).
