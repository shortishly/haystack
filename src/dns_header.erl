-module(dns_header).
-export([
	 decode/1
	]).

decode(<<ID:16, QR:1, OPCODE:4, AA:1, TC:1, RD:1, RA:1, Z:3, RCODE:4, QDCOUNT:16, ANCOUNT:16, NSCOUNT:16, ARCOUNT:16, _/binary>>)  ->
    #{id => ID, qr => QR, opcode => OPCODE, aa => AA, tc => TC, rd => RD, ra => RA, z => Z, rcode => RCODE, qd_count => QDCOUNT, an_count => ANCOUNT, ns_count => NSCOUNT, ar_count => ARCOUNT}.
