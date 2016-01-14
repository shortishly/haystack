-module(dns_rr_soa).
-export([
	 decode/2
	]).

decode(Data, Packet) ->
    {MNAME, Remainder} = dns_rr:labels(Data, Packet),
    {RNAME, <<Serial:32, Refresh:32, Retry:32, Expire:32, Minimum:32>>} = dns_rr:labels(Remainder, Packet),
    #{m_name => MNAME, r_name => RNAME, serial => Serial, refresh => Refresh, retry => Retry, expire => Expire, minimum => Minimum}.




