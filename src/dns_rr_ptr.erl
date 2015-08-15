-module(dns_rr_ptr).
-export([
	 decode/2
	]).

decode(PointerName, Packet) ->
    {Labels, <<>>} = dns_rr:labels(PointerName, Packet),
    #{name => Labels}.
