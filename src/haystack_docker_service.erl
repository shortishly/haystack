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

-module(haystack_docker_service).
-export([add/5]).
-export([remove/1]).

-on_load(on_load/0).

-record(?MODULE, {
           id :: haystack_docker:id(),
           name :: list(binary()),
           class :: atom(),
           type :: atom(),
           ttl :: pos_integer(),
           data :: term()
          }).

on_load() ->
    haystack_table:reuse(?MODULE).

r(Id, Name, Class, Type, TTL, Data) ->
    #?MODULE{id = Id,
             name = Name,
             class = Class,
             type = Type,
             ttl = TTL,
             data = Data}.

add(Id, Private, Type, Name, Origin) ->
    Class = in,
    TTL = haystack_docker_util:ttl(),
    Data = #{priority => priority(),
        weight => weight(),
        port => Private,
        target => Origin
       },

    try
        haystack_node:add(
          [<<"_", (haystack_inet_service:lookup(Private, Type))/binary>>,
           <<"_", Type/binary>> | Name],
          Class,
          srv,
          TTL,
          Data),

        ets:insert(?MODULE, [r(Id, Name, Class, srv, TTL, Data)]),

        lists:foreach(fun
                          (Address) ->
                              haystack_node:add(Name, in, a, TTL, Address)
                      end,
                      haystack_inet:getifaddrs(v4))
    catch _:badarg ->
            no_service_name_for_port
    end.


remove(Id) ->
    lists:foreach(fun
                      (#?MODULE{
                           name = Name,
                           class = Class,
                           type = Type,
                           ttl = TTL,
                           data = Data
                          }) ->
                          haystack_node:remove(Name, Class, Type, TTL, Data)
                  end,
                  ets:take(?MODULE, Id)).



priority() ->
    100.

weight() ->
    100.
