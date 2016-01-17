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
-export([add/2]).
-export([lookup/1]).
-export([remove/1]).
-export([process/1]).

-on_load(on_load/0).

-record(?MODULE, {
           id :: haystack_docker:id(),
           addr :: inet:ip_address()
          }).

on_load() ->
    haystack_table:reuse(?MODULE).

add(Id, Address) ->
    ets:insert(?MODULE, [r(Id, Address)]) orelse
        error({badarg, [Id, Address]}),
    register_container_a(Id, Address).

lookup(Id) ->
    case ets:lookup(?MODULE, Id) of
        [Match] ->
            to_map(Match);

        [] ->
            error({badarg, Id})
    end.

remove(Id) ->
    ets:take(?MODULE, Id).

r(Id, Addr) ->
    #?MODULE{id = Id, addr = Addr}.

to_map(#?MODULE{addr = Addr}) ->
    #{addr => Addr}.


register_container_a(Id, Address) ->
    haystack_node:add(
      haystack_docker_util:container_id(Id),
      in,
      a,
      haystack_docker_util:ttl(),
      Address).


process(Containers) ->
    lists:foreach(fun process_container/1, Containers).


process_container(#{<<"Id">> := Id,
                    <<"Image">> := Image,
                    <<"Names">> := Names,
                    <<"Ports">> := Ports}) ->

    lists:foreach(fun
                      (#{<<"PrivatePort">> := Private,
                         <<"Type">> := Type}) ->
                          add_service(Id,
                                      Names,
                                      Image,
                                      Private,
                                      Type);

                      ({PortProtocol, _}) ->
                          [Private, Type] = binary:split(PortProtocol, <<"/">>),
                          add_service(Id,
                                      Names,
                                      Image,
                                      binary_to_integer(Private),
                                      Type)
                  end,
                  Ports).


add_service(Id, Names, Image, Private, Type) ->
    haystack_docker_service:add(Id,
                                Private,
                                Type,
                                container_name(Names, Image),
                                haystack_docker_util:container_id(Id)).


container_name([<<"/", Name/binary>>], Image) ->
    container_name([Name], Image);
container_name([Name], Image) ->
    Common = [image_name(Image) |
              haystack_name:labels(haystack_config:origin(services))],
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
     haystack_name:labels(haystack_config:origin(services))].


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
