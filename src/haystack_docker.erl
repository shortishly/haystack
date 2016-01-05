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
-module(haystack_docker).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([terminate/2]).

-on_load(on_load/0).

-include_lib("kernel/include/inet.hrl").

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



on_load() ->
    haystack_table:new(?MODULE, bag).

-record(?MODULE, {
           id,
           name,
           class,
           type,
           ttl,
           data
           }).

r(Id, Name, Class, Type, TTL, Data) ->
    #?MODULE{id = Id,
             name = Name,
             class = Class,
             type = Type,
             ttl = TTL,
             data = Data}.

init([]) ->
    case connection() of
        {ok, #{name := Name, port := Port, cert := Cert, key := Key}} ->
            case gun:open(Name,
                          Port,
                          #{transport => ssl,
                            transport_opts => [{certfile, Cert},
                                               {keyfile, Key}]}) of
                {ok, Pid} ->
                    {ok, #{docker => Pid,
                           name => Name,
                           port => Port,
                           certfile => Cert,
                           keyfile => Key,
                           inspection => #{}}};

                {error, Reason} ->
                    {stop, Reason}
            end;

        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(_, _, State) ->
    {stop, error, State}.

handle_cast(_, State) ->
    {stop, error, State}.

handle_info({gun_up, Gun, http}, #{docker := Gun} = State) ->
    {noreply,
     State#{
       info => gun:get(Gun, "/info"),
       containers => gun:get(Gun, "/containers/json"),
       events => gun:get(Gun, "/events")
      }};

handle_info({gun_down, Gun, http, normal, [], []}, #{docker := Gun} = State) ->
    {noreply, State};

handle_info({gun_data, _, Info, fin, Data},
            #{info := Info, name := Name} = State) ->
    #{<<"ID">> := Id} = jsx:decode(Data, [return_maps]),

    case inet:parse_ipv4_address(Name) of
        {ok, Address} ->
            register_docker(Id, Address);

        {error, einval} ->
            {ok,
             #hostent{h_addr_list = Addresses}} = inet_res:getbyname(Name, a),
            lists:foreach(fun
                              (Address) ->
                                  register_docker(Id, Address)
                          end,
                          Addresses)
    end,
    {noreply, maps:without([info], State#{id => Id})};

handle_info({gun_data, _, Containers, fin, Data},
            #{containers := Containers} = State) ->
    {noreply,
     lists:foldl(fun register_container/2,
                 maps:without([containers], State),
                 jsx:decode(Data, [return_maps]))};

handle_info({gun_data, _, Events, nofin, Data}, #{events := Events} = State) ->
    {noreply, event(jsx:decode(Data, [return_maps]), State)};

handle_info({gun_response, _, Info, nofin, 200, _},
            #{info := Info} = State) ->
    {noreply, State};

handle_info({gun_response, _, Containers, nofin, 200, _},
            #{containers := Containers} = State) ->
    {noreply, State};

handle_info({gun_response, _, Events, nofin, 200, _},
            #{events := Events} = State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

connection() ->
    case {haystack:get_env(docker_host), haystack:get_env(docker_cert_path)} of
        {undefined, _} ->
            {error, {missing, "DOCKER_HOST"}};

        {_, undefined} ->
            {error, {missing, "DOCKER_CERT_PATH"}};

        {URI, CertPath} ->
            case http_uri:parse(URI) of
                {ok, {_, _, Name, Port, _, _}} ->
                    {ok, #{name => Name,
                           cert => filename:join(CertPath, "cert.pem"),
                           key => filename:join(CertPath, "key.pem"),
                           port => Port}};

                {error, _} = Error ->
                    Error
            end
    end.

register_container(#{<<"Id">> := Id,
                     <<"Image">> := Image,
                     <<"Ports">> := Ports},
                   #{id := DockerId} = State) ->
    lists:foreach(fun
                      (#{<<"PrivatePort">> := Private,
                         <<"PublicPort">> := Public,
                         <<"Type">> := Type}) ->
                          register_container(Id,
                                             Private,
                                             Public,
                                             Type,
                                             Image,
                                             docker_name(DockerId));

                      (#{<<"PrivatePort">> := _, <<"Type">> := _}) ->
                          nop;

                      ({PortProtocol, [#{<<"HostPort">> := Public}]}) ->
                          [Private, Type] = binary:split(PortProtocol, <<"/">>),
                          register_container(Id,
                                             binary_to_integer(Private),
                                             binary_to_integer(Public),
                                             Type,
                                             Image,
                                             docker_name(DockerId))
                  end,
                  Ports),
    State.


register_container(Id, Private, Public, Type, Image, Origin) ->
    #{Private := Service} = services(),

    Name = [<<"_", Service/binary>>,
            <<"_", Type/binary>>,
            name(Image) | labels(haystack_config:origin(services))],
    Class = in,
    TTL = ttl(),
    Data = #{priority => priority(),
        weight => weight(),
        port => Public,
        target => Origin
       },

    haystack_node:add(
      Name,
      Class,
      srv,
      TTL,
      Data),

    ets:insert(?MODULE, [r(Id, Name, Class, srv, TTL, Data)]).

unregister_container(Id) ->
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


ttl() ->
    100.

priority() ->
    100.

weight() ->
    100.

name(Image) ->
    case binary:split(Image, <<"/">>) of
        [Image] ->
            Image;
        [_, NameVersion] ->
            [Name, _Version] = binary:split(NameVersion, <<":">>),
            Name
    end.

services() ->
    #{
       80 => <<"http">>,
       443 => <<"https">>,
       3535 => <<"domain">>,
       8080 => <<"https">>
     }.

event(#{<<"id">> := Id, <<"status">> := <<"stop">>}, State) ->
    unregister_container(Id),
    State;

event(#{<<"id">> := Id, <<"status">> := <<"start">>},
      #{name := Name,
        port := Port,
        certfile := Cert,
        keyfile := Key} = State) ->

    URL = binary_to_list(iolist_to_binary(["https://",
                                           Name,
                                           ":",
                                           integer_to_list(Port),
                                           "/containers/",
                                           Id,
                                           "/json"])),
    case httpc:request(get, {URL, []},
                       [{ssl, [{certfile, Cert},
                               {keyfile, Key}]}],
                       [{body_format, binary}]) of

        {ok, {{_, 200, _}, _, Body}} ->

            JSON = jsx:decode(Body, [return_maps]),

            #{<<"Config">> := #{<<"Image">> := Image},
              <<"Id">> := Id,
              <<"NetworkSettings">> := #{<<"Ports">> := Ports}} = JSON,

            register_container(#{<<"Image">> => Image,
                                 <<"Id">> => Id,
                                 <<"Ports">> => maps:to_list(Ports)},
                               State);

        {error, _Reason} ->
            whatever
    end,
    State;

event(_, State) ->
    State.

labels(DomainName) ->
    binary:split(DomainName, <<".">>, [global]).

register_docker(Name, Address) ->
    haystack_node:add(
      docker_name(Name),
      in,
      a,
      ttl(),
      Address).

docker_name(Name) ->
      labels(<<
               "d",
               (hash(Name))/binary,
               ".",
               (haystack_config:origin(dockers))/binary
             >>).

hash(Name) ->
    list_to_binary(
      string:to_lower(
        integer_to_list(erlang:phash2(Name), 26))).
