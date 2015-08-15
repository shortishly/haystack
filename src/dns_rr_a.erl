-module(dns_rr_a).
-export([
	 decode/2
	]).

decode(<<IP1:8, IP2:8, IP3:8, IP4:8>>, _) ->
    {IP1, IP2, IP3, IP4}.

