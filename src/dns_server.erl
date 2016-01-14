-module(dns_server).
-behaviour(gen_server).

%% API.
-export([
	 start_link/0
	]).

%% gen_server.
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
    case gen_udp:open(list_to_integer(dns:get_env(port)), [binary]) of
	{ok, Socket} ->
	    {ok, #{socket => Socket}};

	{error, _} = Error ->
	    {stop, Error, #{}}
    end.

handle_call(_, _, State) ->
    {stop, error, State}.

handle_cast(_, State) ->
    {stop, error, State}.


handle_info({udp, _, _, _, <<ID:16, QR:1, OPCODE:4, AA:1, TC:1, RD:1, RA:1, Z:3, RCODE:4, QDCOUNT:16, ANCOUNT:16, NSCOUNT:16, ARCOUNT:16, Remainder/binary>>}, State) ->
    error_logger:info_report([{id, ID},
			      {qr, QR},
			      {opcode, OPCODE},
			      {aa, AA},
			      {tc, TC},
			      {rd, RD},
			      {ra, RA},
			      {z, Z},
			      {rcode, RCODE},
			      {qdcount, QDCOUNT},
			      {ancount, ANCOUNT},
			      {nscount, NSCOUNT},
			      {arcount, ARCOUNT},
			      {remainder, Remainder},
			      {optional, optional(QDCOUNT, ANCOUNT, NSCOUNT, ARCOUNT, Remainder)}]),
    {stop, error, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.


optional(QDCOUNT, ANCOUNT, NSCOUNT, ARCOUNT, Remainder) ->
    {Questions, AnswerRemainder} = questions(QDCOUNT, Remainder),
    {Answers, AuthorityRemainder} = resources(ANCOUNT, AnswerRemainder),
    {Authority, AdditionalRemainder} = resources(NSCOUNT, AuthorityRemainder),     
    {Additional, <<>>} = resources(ARCOUNT, AdditionalRemainder),
    #{questions => Questions, answers => Answers, authority => Authority, additional => Additional}.



questions(QDCOUNT, Questions) ->
    questions(QDCOUNT, Questions, []).     

questions(0, Remainder, A) ->
    {A, Remainder};
questions(N, Questions, A) ->
    {Question, Remainder} = question(Questions, []),
    questions(N-1, Remainder, [Question | A]).

question(<<0:8, QTYPE:16, QCLASS:16, Remainder/binary>>, Labels) ->
    {#{type => QTYPE, class => QCLASS, name => lists:reverse(Labels)}, Remainder};
question(<<Length:8, Label:Length/bytes, Remainder/binary>>, Labels) ->
    question(Remainder, [Label | Labels]).

resources(0, Remainder) ->
    {[], Remainder}.
    




