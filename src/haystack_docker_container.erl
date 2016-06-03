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

-module(haystack_docker_container).
-export([add/6]).
-export([all/0]).
-export([lookup/1]).
-export([remove/1]).
-export([process/1]).

-on_load(on_load/0).

-record(?MODULE, {
           id,
           addr :: inet:ip_address(),
           network,
           endpoint,
           mac_address,
           name
          }).

on_load() ->
    crown_table:reuse(?MODULE, bag).

all() ->
    [to_map(Container) || Container <- ets:tab2list(?MODULE)].

add(Id, Address, Network, Endpoint, MacAddress, Name) ->
    ets:insert(
      ?MODULE, [r(Id, Address, Network, Endpoint, MacAddress, Name)]) orelse
        error({badarg, [Id, Address]}),
    register_container(Id, Address).

lookup(Id) ->
    case ets:lookup(?MODULE, Id) of
        [] ->
            error({badarg, Id});

        Matches ->
            [to_map(Match) || Match <- Matches]
    end.


remove(Id) ->
    haystack_docker_service:remove(Id),
    lists:foreach(
      fun
          (#?MODULE{addr = Addr}) ->
              unregister_container(Id, Addr)
      end,
      ets:take(?MODULE, Id)).


r(Id, Addr, Network, Endpoint, MacAddress, Name) ->
    #?MODULE{id = Id,
             addr = Addr,
             network = Network,
             endpoint = Endpoint,
             mac_address = MacAddress,
             name = Name}.


to_map(
  #?MODULE{id = Id,
           addr = Addr,
           network = Network,
           endpoint = Endpoint,
           mac_address = MacAddress,
           name = Name}) ->
    #{id => Id,
      addr => Addr,
      network => Network,
      endpoint => Endpoint,
      mac_address => MacAddress,
      name => Name}.


register_container(Id, Address) ->
    register_container_a(Id, Address),
    register_container_ptr(Id, Address).


register_container_a(Id, Address) ->
    dns_node:add(
      haystack_docker_util:container_id(Id),
      in,
      a,
      haystack_docker_util:ttl(),
      Address).


register_container_ptr(Id, {IP1, IP2, IP3, IP4}) ->
    dns_node:add(
      [integer_to_binary(IP4),
       integer_to_binary(IP3),
       integer_to_binary(IP2),
       integer_to_binary(IP1),
       <<"in-addr">>,
       <<"arpa">>],
      in,
      ptr,
      haystack_docker_util:ttl(),
      haystack_docker_util:container_id(Id)).


unregister_container(Id, Address) ->
    unregister_container_a(Id, Address),
    unregister_container_ptr(Id, Address).


unregister_container_a(Id, Address) ->
    dns_node:remove(
      haystack_docker_util:container_id(Id),
      in,
      a,
      haystack_docker_util:ttl(),
      Address).


unregister_container_ptr(Id, {IP1, IP2, IP3, IP4}) ->
    dns_node:remove(
      [integer_to_binary(IP4),
       integer_to_binary(IP3),
       integer_to_binary(IP2),
       integer_to_binary(IP1),
       <<"in-addr">>,
       <<"arpa">>],
      in,
      ptr,
      haystack_docker_util:ttl(),
      haystack_docker_util:container_id(Id)).


process(Containers) ->
    lists:foreach(fun process_container/1, Containers).


process_container(#{<<"Id">> := Id,
                    <<"Image">> := Image,
                    <<"Names">> := Names,
                    <<"Ports">> := Ports}) ->

    lists:foreach(
      fun
          (#{<<"PrivatePort">> := Private,
             <<"Type">> := Type}) ->
              add_service(Id, Names, Image, Private, Type);

          ({PortProtocol, _}) ->
              [Private, Type] = binary:split(PortProtocol, <<"/">>),
              add_service(Id, Names, Image, binary_to_integer(Private), Type)
      end,
      Ports).


add_service(Id, Names, Image, Private, Type) ->
    haystack_docker_service:add(
      Id,
      Private,
      Type,
      container_name(Names, Image),
      haystack_docker_util:container_id(Id)).


container_name([<<"/", Name/binary>>], Image) ->
    container_name([Name], Image);
container_name([Name], Image) ->
    Common = [image_name(Image) |
              dns_name:labels(haystack_config:origin(services))],
    try
        case binary:split(Name, <<"-">>, [global]) of
            [Prefix, Suffix] ->
                _ = binary_to_integer(Suffix),
                [Prefix | Common];
            [_] ->
                Common
        end
    catch _:badarg ->
            Common
    end;
container_name(_, Image) ->
    [image_name(Image) |
     dns_name:labels(haystack_config:origin(services))].


image_name(Image) ->
    case binary:split(Image, <<"/">>) of
        [Image] ->
            Image;

        [_, NameVersion] ->
            case binary:split(NameVersion, <<":">>) of
                [Name, _Version] ->
                    Name;

                [Name] ->
                    Name
            end
    end.
