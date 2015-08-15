-module(dns_rr_mx).
-export([
	 decode/2
	]).

decode(<<Preference:16, Exchange/binary>>, Packet) ->
    {Labels, <<>>} = dns_rr:labels(Exchange, Packet),
    #{preference => Preference, exchange => Labels}.
