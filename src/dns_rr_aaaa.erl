-module(dns_rr_aaaa).
-export([
	 decode/2
	]).

decode(<<IP:128>>, _) ->
    IP.

