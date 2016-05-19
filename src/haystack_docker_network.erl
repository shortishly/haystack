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

-module(haystack_docker_network).
-export([is_global/1]).
-export([lookup/1]).
-export([process/1]).

-on_load(on_load/0).

-record(?MODULE, {
           id,
           name,
           scope,
           driver,
           gateway,
           subnet
          }).

on_load() ->
    crown_table:reuse(?MODULE).

add(Id, Name, Scope, Driver, Gateway, Subnet) ->
    ets:insert(?MODULE, [r(Id, Name, Scope, Driver, Gateway, Subnet)]).

lookup(Id) ->
    ets:lookup(?MODULE, Id).

is_global(AddressAndMask) ->
    case ets:match_object(
           ?MODULE,
           r('_',
             '_',
             <<"global">>,
             '_',
             '_',
             haystack_inet:network(AddressAndMask))) of

        [] ->
            false;

        [#?MODULE{}] ->
            true
    end.

r(Id, Name, Scope, Driver, Gateway, Subnet) ->
    #?MODULE{id = Id,
             name = Name,
             scope = Scope,
             driver = Driver,
             gateway = Gateway,
             subnet = Subnet}.

process(Networks) ->
    process(<<"local">>, process(<<"global">>, #{}, Networks), Networks).

process(Scope, Acc0, Networks) ->
    lists:foldl(
      fun
          (#{<<"Id">> := Id,
             <<"Name">> := Name,
             <<"Driver">> := Driver,
             <<"IPAM">> := #{<<"Config">> := Config},
             <<"Containers">> := Containers}, A) ->

              case Config of
                  [#{<<"Gateway">> := Gateway,
                     <<"Subnet">> := Subnet}] ->
                      add(Id,
                          Name,
                          Scope,
                          Driver,
                          haystack_inet:cidr(Gateway),
                          haystack_inet:cidr(Subnet));

                  [#{<<"Subnet">> := Subnet}] ->
                      add(Id,
                          Name,
                          Scope,
                          Driver,
                          undefined,
                          haystack_inet:cidr(Subnet));

                  [] ->
                      add(Id, Name, Scope, Driver, undefined, undefined)
              end,
              maps:merge(
                maps:fold(network_container(Id), #{}, Containers), A)
      end,
      Acc0,
      lists:filter(
        is_scope(Scope),
        Networks)).

is_scope(Scope) ->
    fun
        (#{<<"Scope">> := S}) ->
            S == Scope;
        (_)  ->
            false
    end.



network_container(Network) ->
    fun
        (Container, #{<<"IPv4Address">> := AddressWithMask,
                      <<"EndpointID">> := Endpoint,
                      <<"MacAddress">> := MacAddress,
                      <<"Name">> := Name}, A) ->

            case maps:find(Container, A) of
                error ->
                    [Address, _Mask] = binary:split(AddressWithMask, <<"/">>),
                    case inet:parse_address(binary_to_list(Address)) of
                        {ok, IP} ->
                            haystack_docker_container:add(
                              Container,
                              IP,
                              Network,
                              Endpoint,
                              MacAddress,
                              Name),
                            A#{Container => IP};

                        {error, _} ->
                            A
                    end;

                {ok, _} ->
                    A
            end
    end.
