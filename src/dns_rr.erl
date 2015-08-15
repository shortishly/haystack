-module(dns_rr).
-export([
	 decode/2,
	 labels/2
	]).

decode(<<>>, _) ->
    {#{}, <<>>};
decode(Resource, Packet) ->
    {Labels, <<Type:16, Class:16, TTL:32, Length:16, Data:Length/bytes, Remainder/binary>>} = labels(Resource, Packet),
    {resource(Type, Class, TTL, Data, Labels, Packet), Remainder}.

resource(Type, Class, TTL, Data, Labels, Packet) ->
    case maps:find(Type, dns:get_env(resources)) of
	{ok, #{name := Name, decoder := Decoder}} ->
	    #{type => Name, class => Class, ttl => TTL, data => Decoder:decode(Data, Packet), labels => Labels};
	error ->
	    #{type => Type, class => Class, ttl => TTL, data => Data, labels => Labels}
    end.

labels(Data, Packet) ->
    {Labels, Remainder} = labels(Data, Packet, []),
    {lists:reverse(Labels), Remainder}.

labels(<<1:1, 1:1, Offset:14, Remainder/binary>>, Packet, A) ->
    <<_:Offset/bytes, Pointer/binary>> = Packet,
    {Labels, _} = labels(Pointer, Packet, A),
    {Labels, Remainder};
labels(<<0:8, Remainder/binary>>, _, A) ->
    {A, Remainder};
labels(<<Length:8, Label:Length/bytes, Remainder/binary>>, Packet, A) ->
    labels(Remainder, Packet, [Label | A]).
