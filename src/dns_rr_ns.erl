-module(dns_rr_ns).
-export([
	 decode/2
	]).

decode(Data, Packet) ->
    {Labels, <<>>} = dns_rr:labels(Data, Packet),
    Labels.
