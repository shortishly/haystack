-module(dns_rr_srv).
-export([
	 decode/2
	]).

decode(<<Priority:16, Weight:16, Port:16, Target/binary>>, Packet) ->
    {Labels, <<>>} = dns_rr:labels(Target, Packet),
    #{priority => Priority, weight => Weight, port => Port, target => Labels}.




