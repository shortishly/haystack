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

-module(haystack_api_info_resource).

-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([init/2]).
-export([options/2]).
-export([services/0]).
-export([to_json/2]).
-export([applications/0]).
-export([containers/0]).
-export([info/0]).
-export([networks/0]).

init(Req, _) ->
    {cowboy_rest, cors:allow_origin(Req), #{}}.

allowed_methods(Req, State) ->
    {allowed(), Req, State}.

options(Req, State) ->
    cors:options(Req, State, allowed()).

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

allowed() ->
    [<<"GET">>,
     <<"HEAD">>,
     <<"OPTIONS">>].

to_json(Req, State) ->
    {jsx:encode(info()), Req, State}.

info() ->
    #{applications => applications(),
      containers => containers(),
      networks => networks(),
      services => services(),
      proxy => munchausen_http_proxy_resource:metrics(),
      version => version()}.

applications() ->
    lists:foldl(
      fun
          ({Application, _, VSN}, A) ->
              A#{Application => any:to_binary(VSN)}
      end,
      #{},
      application:which_applications()).

containers() ->
    lists:foldl(
      fun
          (#{id := Id, addr := Addr} = Container, A) ->
              A#{Id => maps:without([id], Container#{addr := any:to_binary(inet:ntoa(Addr))})}
      end,
      #{},
      haystack_docker_container:all()).

networks() ->
    lists:foldl(
      fun
          (#{id := Id, gateway := #{address := Gateway}, subnet := #{address := Address, mask := Mask}} = Network, A) ->
              A#{Id => drop_undefined(
                         maps:without(
                           [id],
                           Network#{
                             gateway := any:to_binary(inet:ntoa(Gateway)),
                             subnet := <<(any:to_binary(inet:ntoa(Address)))/bytes,
                                                "/",
                                                (any:to_binary(Mask))/bytes>>}))};
          
          (#{id := Id, subnet := #{address := Address, mask := Mask}} = Network, A) ->
              A#{Id => drop_undefined(
                         maps:without(
                           [id],
                           Network#{
                             subnet := <<(any:to_binary(inet:ntoa(Address)))/bytes,
                                                "/",
                                                (any:to_binary(Mask))/bytes>>}))};
          
          (#{id := Id} = Network, A) ->
              A#{Id => drop_undefined(maps:without([id], Network))}
      end,
      #{},
      haystack_docker_network:all()).

drop_undefined(M) ->
    maps:filter(
      fun
          (_, undefined) ->
              false;
          (_, _) ->
              true
      end,
      M).

services() ->
    lists:foldl(
      fun

          (#{name := Name, target := Target} = Service, Services) ->
              Id = uri(Service),

              case Services of
                  #{Id := Existing} ->
                      Services#{Id := [Service#{name := dns_name(Name), target := dns_name(Target)} | Existing]};

                  #{} ->
                      Services#{Id => [Service#{name := dns_name(Name), target := dns_name(Target)}]}
              end

      end,
      #{},
      haystack_docker_service:all()).

uri(#{name := Name, port := Port, type := Type}) ->
    <<Type/bytes, "://", (dns_name(Name))/bytes, ":", (any:to_binary(Port))/bytes>>.


dns_name(Names) ->
    lists:foldr(
      fun
          (Name, <<>>) ->
              Name;
          (Name, A) ->
              <<Name/bytes, ".", A/bytes>>
      end,
      <<>>,
      Names).

version() ->
    [Major, Minor, Patch] = string:tokens(haystack:vsn(), "."),
    #{major => any:to_integer(Major),
      minor => any:to_integer(Minor),
      patch => any:to_integer(Patch)}.
