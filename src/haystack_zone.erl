%% Copyright (c) 2015-2016 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(haystack_zone).
-export([parse/1]).
-export([process/1]).
-export([strip/1]).


process(Zone) ->
    case parse(Zone) of
        {ok, Statements} ->
            lists:foldl(fun process/2, #{}, Statements),
            ok;

        {error, {Line, Module, Message}} ->
            {error, #{line => Line, module => Module, message => Message}}
    end.


parse(Zone) when is_binary(Zone) ->
    parse(binary_to_list(Zone));
parse(Zone) ->
    {ok, Tokens, _} = haystack_zone_leexer:string(Zone),
    haystack_zone_grammar:parse(strip(Tokens)).


%% strip out single or multi-line parenthesis lines collapsing any
%% resource data into a single line

strip(Tokens) ->
    cleanup(outside(Tokens, [])).

cleanup(Tokens) ->
    cleanup(Tokens, []).

cleanup([{ws, _, _}, {eol, _} = H | T], A) ->
    cleanup([H | T], A);
cleanup([{ws, _, _}, {ws, _, _} = H | T], A) ->
    cleanup([H | T], A);
cleanup([{eol, _}, {eol, _} = H | T], A) ->
    cleanup([H | T], A);
cleanup([H | T], A) ->
    cleanup(T, [H | A]);
cleanup([], A) ->
    lists:reverse(A).

outside([{lparen, _} | T], A) ->
    inside(T, A);
outside([H | T], A) ->
    outside(T, [H | A]);
outside([], A) ->
    lists:reverse(A).

inside([{comment, _, _}, {eol, _} | T], A) ->
    inside(T, A);
inside([{rparen, _} | T], A) ->
    outside(T, A);
inside([{eol, _} | T], A) ->
    inside(T, A);
inside([H | T], A) ->
    inside(T, [H | A]).


process(#{statement := #{control := origin, domain := Domain}}, A) ->
    A#{origin => Domain};

process(#{statement := #{control := ttl, duration := Duration}}, A) ->
    A#{duration => Duration};

process(#{statement := #{domain := origin, rr := Resource}},
        #{origin := Domain} = A) ->
    resource(Domain, 100, Resource),
    A#{domain => Domain};

process(#{statement := #{domain := Sub, rr := Resource}},
        #{origin := Origin} = A) ->
    Domain = domain(Sub, Origin),
    resource(Domain, 100, Resource),
    A#{domain => Domain};

process(#{statement := #{rr := Resource}}, #{domain := Domain} = A) ->
    resource(Domain, 100, Resource),
    A.

name(Sub, Domain) ->
    #{absolute := Absolute} = domain(Sub, Domain),
    Absolute.

name(#{absolute := Absolute}) ->
    Absolute.

domain(#{relative := Sub}, #{absolute := FQDN}) ->
    #{absolute => Sub ++ FQDN};
domain(#{absolute := _} = FQDN, _) ->
    FQDN.


resource(Domain, TTL, #{type :=Type,
                        class := Class,
                        rdata := #{domain := Name}}) when Type == ns orelse
                                                          Type == cname ->
    haystack_node:add(name(Domain), Class, Type, TTL, name(Name, Domain));

resource(Domain, TTL, #{type := a = Type,
                        class := Class,
                        rdata := #{ip := IP}}) ->
    haystack_node:add(name(Domain), Class, Type, TTL, IP);

resource(Domain, TTL, #{type := mx = Type,
                        class := Class,
                        rdata := #{domain := Mail,
                                   preference := Preference}}) ->
    haystack_node:add(name(Domain),
                      Class,
                      Type,
                      TTL,
                      #{exchange => name(Mail, Domain),
                        preference => Preference});

resource(Domain, TTL, #{type := soa = Type,
                        class := Class,
                        rdata :=  #{expire := Expire,
                                    m_name := MName,
                                    minimum := Minimum,
                                    r_name := RName,
                                    refresh := Refresh,
                                    retry := Retry,
                                    serial := Serial}}) ->
    haystack_node:add(name(Domain),
                      Class,
                      Type,
                      TTL,
                      #{m_name => name(MName, Domain),
                        r_name => name(RName, Domain),
                        serial => Serial,
                        refresh => duration(Refresh),
                        retry => duration(Retry),
                        expire => duration(Expire),
                        minimum => duration(Minimum)}).


duration(#{weeks := Weeks}) ->
    duration(#{days => Weeks*7});
duration(#{days := Days}) ->
    duration(#{hours => Days*24});
duration(#{hours := Hours}) ->
    duration(#{minutes => Hours*60});
duration(#{minutes := Minutes}) ->
    duration(#{seconds => Minutes*60});
duration(#{seconds := Seconds}) ->
    Seconds.
