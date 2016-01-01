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

Definitions.

At = @
Comment = ;.*
Escape = \\[^0-9]
Octet = \\[0-9][0-9][0-9]
LeftParen = \(
RightParen = \)
Decimal = [0-9]+
Duration = [mhdw]
Class = (IN|CS|CH|HS)
Type = (A|NS|CNAME|SOA|PTR|MX|TXT|AAAA|SRV|OPT|TSIG)
A = A
NS = NS
CNAME = CNAME
SOA = SOA
PTR = PTR
MX = MX
TXT = TXT
AAAA = AAAA
SRV = SRV
OPT = OPT
TSIG = TSIG
Name = [a-zA-Z][a-zA-Z0-9]*
WS = [\s\t]+
EOL = [\r\n]
Control = \$(ORIGIN|INCLUDE|TTL)
Dot = \.
Colon = :

Rules.

{Colon}+ : {token, {colon, TokenLine, TokenChars}}.
{WS} : {token, {ws, TokenLine, TokenChars}}.
{At} : {token, {origin, TokenLine}}.
{Dot} : {token, {dot, TokenLine}}.
{Comment} : {token, {comment, TokenLine, tl(TokenChars)}}.
{Escape} : {token, {escape, TokenLine, tl(TokenChars)}}.
{Octet} : {token, {octet, TokenLine, list_to_integer(tl(TokenChars), 8)}}.
{LeftParen} : {token, {lparen, TokenLine}}.
{RightParen} : {token, {rparen, TokenLine}}.
{Decimal} : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{Class} : {token, {class, TokenLine, TokenChars}}.
{A} : {token, {a, TokenLine}}.
{NS} : {token, {ns, TokenLine}}.
{CNAME} : {token, {cname, TokenLine}}.
{SOA} : {token, {soa, TokenLine}}.
{PTR} : {token, {ptr, TokenLine}}.
{MX} : {token, {mx, TokenLine}}.
{TXT} : {token, {txt, TokenLine}}.
{AAAA} : {token, {aaaa, TokenLine}}.
{SRV} : {token, {srv, TokenLine}}.
{TSIG} : {token, {tsig, TokenLine}}.
{Name} : {token, {name, TokenLine, TokenChars}}.
{EOL} : {token, {eol, TokenLine}}.
{Control} : {token, {control, TokenLine, to_control(tl(TokenChars))}}.
{Decimal}{Duration} : {token, {duration, TokenLine, to_duration(TokenChars)}}.


Erlang code.

to_duration(Duration) ->
    case string:to_integer(string:to_lower(Duration)) of
        {Value, "m"} ->
            #{minutes => Value};

        {Value, "h"} ->
            #{hours => Value};

        {Value, "d"} ->
            #{days => Value};

        {Value, "w"} ->
            #{weeks => Value}
    end.

to_control("ORIGIN") ->
    origin;
to_control("INCLUDE") ->
    include;
to_control("TTL") ->
    ttl.
