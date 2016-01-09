%% Copyright (c) 2012-2015 Peter Morgan <peter.james.morgan@gmail.com>
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

-module(haystack_protocol).
-export([decode/1]).
-export([encode/1]).
-export([id/1]).
-export([lookup/1]).
-export([process/1]).
-export([qr/1]).

-on_load(on_load/0).

on_load() ->
    haystack_table:new(?MODULE),
    add(0, query, haystack_query),
    add(5, update, haystack_update).

-record(?MODULE, {
           opcode :: integer(),
           name :: atom(),
           coder :: atom()
          }).

add(OPCODE, Name, Coder) ->
    ets:insert_new(?MODULE, r(OPCODE, Name, Coder)) orelse
        error({badarg, [OPCODE, Name, Coder]}),
    ok.


lookup(OPCODE) when is_integer(OPCODE) ->
    case ets:lookup(?MODULE, OPCODE) of
        [#?MODULE{name = Name}] ->
            Name;

        [] ->
            error({badarg, OPCODE})
    end;
lookup(Name) ->
    case ets:match_object(?MODULE, r('_', Name, '_')) of
        [#?MODULE{opcode = OPCODE}] ->
            OPCODE;

        [] ->
            error({badarg, Name})
    end.

process(<<_:16, _:1, OPCODE:4, _:3, _/binary>> = Packet) ->
    (coder(OPCODE)):process(Packet).

decode(<<_:16, _:1, OPCODE:4, _:3, _/binary>> = Packet) ->
    (coder(OPCODE)):decode(Packet).

encode(#{header := #{opcode := OPCODE}} = Packet) ->
    (coder(OPCODE)):encode(Packet).

qr(<<_:16, QR:1, OPCODE:4, _:3, _/binary>>) ->
    haystack_header:lookup(coder(OPCODE), qr, QR).

id(<<Id:16, _/binary>>) ->
    Id.

coder(OPCODE) when is_integer(OPCODE) ->
    case ets:lookup(?MODULE, OPCODE) of
        [#?MODULE{coder = Coder}] ->
            Coder;

        [] ->
            error({badarg, OPCODE})
    end;
coder(Name) ->
    case ets:match_object(?MODULE, r('_', Name, '_')) of
        [#?MODULE{coder = Coder}] ->
            Coder;
        [] ->
            error({badarg, Name})
    end.

r(OPCODE, Name, Coder) ->
    #?MODULE{opcode = OPCODE, name = Name, coder = Coder}.
