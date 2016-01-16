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
-include_lib("public_key/include/public_key.hrl").

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
        {ok, #{host := Host,
               port := Port,
               cert := Cert,
               key := Key}} ->

            case gun:open(Host,
                          Port,
                          #{transport => ssl,
                            transport_opts => [{cert, Cert},
                                               {key, Key}]}) of
                {ok, Pid} ->
                    {ok, #{docker => Pid,
                           monitor => monitor(process, Pid),
                           partial => <<>>,
                           host => Host,
                           port => Port,
                           cert => Cert,
                           key => Key}};

                {error, Reason} ->
                    {stop, Reason}
            end;

        {ok, #{host := Host,
               port := Port}} ->

            case gun:open(Host,
                          Port,
                          #{transport => tcp}) of
                {ok, Pid} ->
                    {ok, #{docker => Pid,
                           monitor => monitor(process, Pid),
                           partial => <<>>,
                           host => Host,
                           port => Port}};

                {error, Reason} ->
                    {stop, Reason}
            end;

        {error, Reason} ->
            error_logger:error_report([{module, ?MODULE},
                                       {line, ?LINE},
                                       {reason, Reason}]),
            ignore
    end.

handle_call(_, _, State) ->
    {stop, error, State}.

handle_cast(_, State) ->
    {stop, error, State}.

handle_info({'DOWN', Monitor, process, _, normal},
            #{monitor := Monitor} = State) ->
    {stop, {error, lost_connection}, State};

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
            #{info := Info,
              partial := Partial,
              host := Host} = State) ->
    #{<<"ID">> := Id,
      <<"SystemTime">> := SystemTime}  = jsx:decode(<<Partial/binary,
                                                      Data/binary>>,
                                                    [return_maps]),

    StartTime = haystack_date:seconds_since_epoch(system_time(SystemTime)),

    case inet:parse_ipv4_address(Host) of
        {ok, Address} ->
            register_docker(Id, Address),
            {noreply,
             maps:without([info], State#{id => Id,
                                         start_time => StartTime,
                                         partial => <<>>})};

        {error, einval} ->
            case inet:gethostbyname(Host) of
                {ok, #hostent{h_addr_list = Addresses}} ->
                    lists:foreach(fun
                                      (Address) ->
                                          register_docker(Id, Address)
                                  end,
                                  Addresses),
                    {noreply,
                     maps:without([info], State#{id => Id,
                                                 start_time => StartTime,
                                                 partial => <<>>})};

                {error, Reason} ->
                    {stop, Reason, State}
            end
    end;

handle_info({gun_data, _, Containers, fin, Data},
            #{containers := Containers,
              partial := Partial} = State) ->
    {noreply,
     lists:foldl(fun register_container/2,
                 maps:without([containers], State#{partial => <<>>}),
                 jsx:decode(<<Partial/binary, Data/binary>>, [return_maps]))};

handle_info({gun_data, _, Events, nofin, Data},
            #{partial := Partial,
              events := Events} = State) ->
    {noreply,
     process_events(<<Partial/binary, Data/binary>>, State#{partial => <<>>})};

handle_info({gun_data, _, _, nofin, Data}, #{partial := Partial} = State) ->
    {noreply, State#{partial => <<Partial/binary, Data/binary>>}};

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
    case {haystack:get_env(docker_host, [os_env]),
          haystack:get_env(docker_cert_path, [os_env]),
          haystack:get_env(docker_cert, [os_env]),
          haystack:get_env(docker_key, [os_env])} of

        {undefined, _, _, _} ->
            {error, {missing, "DOCKER_HOST"}};

        {URI, undefined, undefined, undefined} ->
            connection(URI);

        {_, undefined, _, undefined} ->
            {error, {missing, "DOCKER_KEY"}};

        {_, undefined, undefined, _} ->
            {error, {missing, "DOCKER_CERT"}};

        {URI, _, Cert, Key} when is_list(Cert) andalso is_list(Key) ->
            connection(URI, list_to_binary(Cert), list_to_binary(Key));

        {URI, CertPath, undefined, undefined} ->
            case {read_file(CertPath, "cert.pem"),
                  read_file(CertPath, "key.pem")} of

                {{ok, Cert}, {ok, Key}} ->
                    connection(URI, Cert, Key);

                {{error, _} = Error, _} ->
                    Error;

                {_, {error, _} = Error}->
                    Error
            end
    end.

connection(URI, Cert, Key) ->
    [{KeyType, Value, _}] = public_key:pem_decode(Key),
    [{_, Certificate, _}] = public_key:pem_decode(Cert),
    case connection(URI) of
        {ok, Details} ->
            {ok, Details#{cert => Certificate,
                          key => {KeyType, Value}}};

        {error, _} = Error ->
            Error
    end.


connection(URI) ->
    case http_uri:parse(URI) of
        {ok, {_, _, Host, Port, _, _}} ->
            {ok, #{host => Host, port => Port}};

        {error, _} = Error ->
            Error
    end.


read_file(Path, File) ->
    file:read_file(filename:join(Path, File)).


register_container(#{<<"Id">> := Id,
                     <<"Image">> := Image,
                     <<"Names">> := Names,
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
                                             container_name(Names, Image),
                                             docker_name(DockerId));

                      (#{<<"PrivatePort">> := _, <<"Type">> := _}) ->
                          nop;

                      ({PortProtocol, [#{<<"HostPort">> := Public}]}) ->
                          [Private, Type] = binary:split(PortProtocol, <<"/">>),
                          register_container(Id,
                                             binary_to_integer(Private),
                                             binary_to_integer(Public),
                                             Type,
                                             container_name(Names, Image),
                                             docker_name(DockerId));

                      ({PortProtocol, null}) when is_binary(PortProtocol) ->
                          nop
                  end,
                  Ports),
    State.

container_name([<<"/", Name/binary>>], Image) ->
    container_name([Name], Image);
container_name([Name], Image) ->
    Common = [name(Image) | labels(haystack_config:origin(services))],
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
    [name(Image) | labels(haystack_config:origin(services))].


register_container(Id, Private, Public, Type, Name, Origin) ->
    Class = in,
    TTL = ttl(),
    Data = #{priority => priority(),
        weight => weight(),
        port => Public,
        target => Origin
       },

    try
        haystack_node:add(
          [<<"_", (haystack_inet_service:lookup(Private, Type))/binary>>,
           <<"_", Type/binary>> | Name],
          Class,
          srv,
          TTL,
          Data)
    catch _:badarg ->
            no_service_name_for_port
    end,

    ets:insert(?MODULE, [r(Id, Name, Class, srv, TTL, Data)]),

    lists:foreach(fun
                      (Address) ->
                          haystack_node:add(Name, in, a, ttl(), Address)
                  end,
                  haystack_inet:getifaddrs(v4)).


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
            case binary:split(NameVersion, <<":">>) of
                [Name, _Version] ->
                    Name;

                [Name] ->
                    Name
            end
    end.


process_events(Events, State) ->
    case binary:split(Events, <<"\n">>) of
        [Event, Remainder] ->
            process_events(Remainder,
                           event(jsx:decode(Event, [return_maps]), State));

        [Partial] ->
            State#{partial => Partial};

        [] ->
            State#{partial => <<>>}
    end.

event(#{<<"id">> := Id,
        <<"time">> := Time,
        <<"status">> := <<"stop">>},
      #{start_time := StartTime} = State) when Time > StartTime ->
    unregister_container(Id),
    State;

event(#{<<"id">> := Id,
        <<"time">> := Time,
        <<"status">> := <<"start">>} = Event,
      #{host := Host,
        port := Port,
        cert := Cert,
        start_time := StartTime,
        key := Key} = State) when Time > StartTime ->

    URL = binary_to_list(iolist_to_binary(["https://",
                                           Host,
                                           ":",
                                           integer_to_list(Port),
                                           "/containers/",
                                           Id,
                                           "/json"])),
    case httpc:request(get, {URL, []},
                       [{ssl, [{cert, Cert},
                               {key, Key}]}],
                       [{body_format, binary}]) of

        {ok, {{_, 200, _}, _, Body}} ->

            case jsx:decode(Body, [return_maps]) of
                #{<<"NetworkSettings">> :=  #{<<"Ports">> := null}} ->
                    nothing_to_register;

                #{<<"Config">> := #{<<"Image">> := Image},
                  <<"Id">> := Id,
                  <<"Name">> := Name,
                  <<"NetworkSettings">> := #{<<"Ports">> := Ports}} ->
                    register_container(#{<<"Image">> => Image,
                                         <<"Id">> => Id,
                                         <<"Names">> => [Name],
                                         <<"Ports">> => maps:to_list(Ports)},
                                       State)
            end;

        {ok, {{_, 404, _}, _, _}} ->
            error_logger:info_report([{module, ?MODULE},
                                      {line, ?LINE},
                                      {event, Event},
                                      {reason, not_found},
                                      {host, Host},
                                      {port, Port},
                                      {id, Id},
                                      {url, URL}]);

        {error, Reason} ->
            error_logger:error_report([{module, ?MODULE},
                                       {line, ?LINE},
                                       {reason, Reason},
                                       {host, Host},
                                       {port, Port},
                                       {id, Id},
                                       {url, URL}])
    end,
    State;

event(#{<<"id">> := Id,
        <<"time">> := Time,
        <<"status">> := <<"start">>} = Event,
      #{host := Host,
        start_time := StartTime,
        port := Port} = State) when Time > StartTime ->

    URL = binary_to_list(iolist_to_binary(["http://",
                                           Host,
                                           ":",
                                           integer_to_list(Port),
                                           "/containers/",
                                           Id,
                                           "/json"])),
    case httpc:request(get, {URL, []},
                       [],
                       [{body_format, binary}]) of

        {ok, {{_, 200, _}, _, Body}} ->

            case jsx:decode(Body, [return_maps]) of
                #{<<"NetworkSettings">> :=  #{<<"Ports">> := null}} ->
                    nothing_to_register;

                #{<<"Config">> := #{<<"Image">> := Image},
                  <<"Id">> := Id,
                  <<"Name">> := Name,
                  <<"NetworkSettings">> := #{<<"Ports">> := Ports}} ->
                    register_container(#{<<"Image">> => Image,
                                         <<"Id">> => Id,
                                         <<"Names">> => [Name],
                                         <<"Ports">> => maps:to_list(Ports)},
                                       State)
            end;

        {ok, {{_, 404, _}, _, _}} ->
            error_logger:info_report([{module, ?MODULE},
                                      {line, ?LINE},
                                      {event, Event},
                                      {reason, not_found},
                                      {host, Host},
                                      {port, Port},
                                      {id, Id},
                                      {url, URL}]);

        {error, Reason} ->
            error_logger:error_report([{module, ?MODULE},
                                       {line, ?LINE},
                                       {event, Event},
                                       {reason, Reason},
                                       {host, Host},
                                       {port, Port},
                                       {id, Id},
                                       {url, URL}])
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


%% <<"2016-01-16T11:29:57.061705579Z">>
system_time(<<Year:4/bytes,
              "-",
              Month:2/bytes,
              "-",
              Date:2/bytes,
              "T",
              Hour:2/bytes,
              ":",
              Minute:2/bytes,
              ":",
              Second:2/bytes,
              ".",
              _:9/bytes,
              "Z">>) ->
    {{binary_to_integer(Year),
      binary_to_integer(Month),
      binary_to_integer(Date)},
     {binary_to_integer(Hour),
      binary_to_integer(Minute),
      binary_to_integer(Second)}}.
