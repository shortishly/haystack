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
           priority :: pos_integer(),
           weight :: pos_integer(),
           port :: pos_integer(),
           target :: list(binary())
          }).


on_load() ->
    haystack_table:reuse(?MODULE, bag).


r(Id, Name, Class, Type, TTL, Priority, Weight, Port, Target) ->
    #?MODULE{id = Id,
             name = Name,
             class = Class,
             type = Type,
             ttl = TTL,
             priority = Priority,
             weight = Weight,
             port = Port,
             target = Target}.


add(Id, Private, Type, Name, Origin) ->
    Class = in,
    TTL = haystack_docker_util:ttl(),

    try
        add_srv(Name, Class, Type, TTL, priority(), weight(), Private, Origin),

        ets:insert(?MODULE,
                   [r(Id,
                      Name,
                      Class,
                      Type,
                      TTL,
                      priority(),
                      weight(),
                      Private,
                      Origin)]),


        Addresses = haystack_inet:getifaddrs(v4),

        case lists:any(fun haystack_docker_network:is_global/1, Addresses) of
            true ->
                  lists:foreach(
                    fun
                        (#{address := Address}) ->
                            haystack_node:add(Name, in, a, TTL, Address)
                    end,
                    lists:filter(
                      fun haystack_docker_network:is_global/1,
                      Addresses));

            false ->
                lists:foreach(
                  fun
                      (#{address := Address}) ->
                          haystack_node:add(Name, in, a, TTL, Address)
                  end,
                  Addresses)
        end
    catch _:badarg ->
            no_service_name_for_port
    end.


remove(Id) ->
    lists:foldl(
      fun
          (#?MODULE{
               name = Name,
               class = Class,
               type = Type,
               ttl = TTL,
               priority = Priority,
               weight = Weight,
               port = Private,
               target = Target
              }, []) ->

              case ets:match_object(
                     ?MODULE,
                     r('_', Name, '_', '_', '_', '_', '_', '_', '_')) of

                  [] ->
                      lists:foreach(
                        fun
                            (#{address := Address}) ->
                                haystack_node:remove(Name, in, a, TTL, Address)
                        end,
                        haystack_inet:getifaddrs(v4));

                  _ ->
                      nop
              end,
              remove_srv(
                Name, Class, Type, TTL, Priority, Weight, Private, Target);

          (#?MODULE{
               name = Name,
               class = Class,
               type = Type,
               ttl = TTL,
               priority = Priority,
               weight = Weight,
               port = Private,
               target = Target
              }, _) ->
              remove_srv(
                Name, Class, Type, TTL, Priority, Weight, Private, Target)
      end,
      [],
      ets:take(?MODULE, Id)).


remove_srv(Name, Class, Type, TTL, Priority, Weight, Private, Target) ->
    haystack_node:remove(
      [<<"_", (haystack_inet_service:lookup(Private,Type))/binary>>,
       <<"_", Type/binary>> | Name],
      Class,
      srv,
      TTL,
      #{priority => Priority,
        weight => Weight,
        port => Private,
        target => Target
       }).


add_srv(Name, Class, Type, TTL, Priority, Weight, Private, Target) ->
    haystack_node:add(
      [<<"_", (haystack_inet_service:lookup(Private, Type))/binary>>,
       <<"_", Type/binary>> | Name],
      Class,
      srv,
      TTL,
      #{priority => Priority,
        weight => Weight,
        port => Private,
        target => Target
       }).



priority() ->
    100.


weight() ->
    100.
