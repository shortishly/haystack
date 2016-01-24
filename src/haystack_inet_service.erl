%% Copyright (c) 2016 Peter Morgan <peter.james.morgan@gmail.com>
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

-module(haystack_inet_service).
-export([lookup/2]).

-on_load(on_load/0).

-record(?MODULE, {
           port_protocol,
           service
          }).

on_load() ->
    haystack_table:new(?MODULE, bag),
    try
        {ok, Services} = haystack:priv_read_file(
                           "service-names-port-numbers.csv"),

        ets:insert(?MODULE,
                   lists:foldl(fun(Row, A) ->
                                       case binary:split(
                                              Row, <<",">>, [global]) of

                                           [_] ->
                                               A;

                                           [<<>> | _] ->
                                               A;

                                           [<<"www", _/binary>> | _] ->
                                               A;

                                           [Service, Port, Protocol | _] ->
                                               try
                                                   [r(Service,
                                                      binary_to_integer(Port),
                                                      Protocol) | A]
                                               catch _:badarg ->
                                                       A
                                               end
                                       end
                               end,
                               [],
                               tl(binary:split(Services,
                                               <<"\r\n">>,
                                               [global])))),
        ok
    catch _:badarg ->
            ok
    end.

r(Service, Port, Protocol) ->
    #?MODULE{port_protocol = {Port, Protocol},
             service = Service}.

lookup(Port, Protocol) ->
    case ets:lookup(?MODULE, {Port, Protocol}) of
        [#?MODULE{service = Service}] ->
            Service;
        [] ->
            error(badarg)
    end.
