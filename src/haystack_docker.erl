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

-include_lib("kernel/include/inet.hrl").
-include_lib("public_key/include/public_key.hrl").

%% API.

-export_type([id/0]).

-type id() :: <<_:64>>.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    case connection() of
        {ok, #{host := Host,
               port := Port,
               cert := Cert,
               key := Key}} ->

            case gun:open(Host,
                          Port,
                          #{transport => ssl,
                            transport_opts => haystack_docker_util:ssl(Cert,
                                                                       Key)}) of
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
       info => gun:get(Gun, "/info")
      }};

handle_info({gun_down, Gun, http, normal, [], []}, #{docker := Gun} = State) ->
    {noreply, State};

handle_info({gun_data, Gun, Info, fin, Data},
            #{info := Info,
              partial := Partial,
              host := Host} = State) ->

    case jsx:decode(<<Partial/binary, Data/binary>>, [return_maps]) of

        #{<<"ID">> := <<>>} ->
            {noreply,
             maps:without(
               [info],
               State#{start_time => haystack_date:right_now(),
                      partial => <<>>,
                      networks => gun:get(Gun, "/networks")})};

        #{<<"ID">> := Id,
          <<"SystemTime">> := SystemTime} ->

            StartTime = haystack_date:seconds_since_epoch(
                          haystack_docker_util:system_time(SystemTime)),

            case inet:parse_ipv4_address(Host) of
                {ok, Address} ->
                    register_docker(Id, Address),
                    {noreply,
                     maps:without(
                       [info],
                       State#{id => Id,
                              start_time => StartTime,
                              networks => gun:get(Gun, "/networks"),
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
                             maps:without(
                               [info],
                               State#{
                                 id => Id,
                                 start_time => StartTime,
                                 networks => gun:get(Gun, "/networks"),
                                 partial => <<>>})};
                        {error, Reason} ->
                            {stop, Reason, State}
                    end
            end
    end;

handle_info({gun_data, Gun, Networks, fin, Data},
            #{networks := Networks,
              partial := Partial} = State) ->
    haystack_docker_network:process(<<Partial/binary, Data/binary>>),
    {noreply, maps:without(
                [networks], State#{
                              partial => <<>>,
                              containers => gun:get(Gun, "/containers/json")})};

handle_info({gun_data, Gun, Containers, fin, Data},
            #{containers := Containers,
              partial := Partial} = State) ->
    process_containers(<<Partial/binary, Data/binary>>),
    {noreply, maps:without(
                [containers],
                State#{partial => <<>>,
                       events => gun:get(Gun, "/events")})};

handle_info({gun_data, _, Events, nofin, Data},
            #{partial := Partial,
              events := Events} = State) ->
    {noreply, process_events(
                <<Partial/binary, Data/binary>>, State#{partial => <<>>})};

handle_info({gun_data, _, _, nofin, Data}, #{partial := Partial} = State) ->
    {noreply, State#{partial => <<Partial/binary, Data/binary>>}};

handle_info({gun_response, _, Info, nofin, 200, _},
            #{info := Info} = State) ->
    {noreply, State};

handle_info({gun_response, _, Networks, nofin, 200, _},
            #{networks := Networks} = State) ->
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


process_containers(Containers) ->
    haystack_docker_container:process(jsx:decode(Containers, [return_maps])).

process_events(Events, State) ->
    case {binary:match(Events, <<"\n">>), binary:match(Events, <<"}{">>)} of
        {nomatch, nomatch} ->
            State#{partial => Events};

        {_, nomatch} ->
            case binary:split(Events, <<"\n">>) of
                [Event, Remainder] ->
                    process_events(Remainder,
                                   haystack_docker_event:process(
                                     jsx:decode(Event, [return_maps]), State));

                [Partial] ->
                    State#{partial => Partial};

                [] ->
                    State#{partial => <<>>}
            end;

        {nomatch, _} ->
            case binary:split(Events, <<"}{">>) of
                [Event, Remainder] ->
                    process_events(<<"{", Remainder/binary>>,
                                   haystack_docker_event:process(
                                     jsx:decode(<<Event/binary, "}">>,
                                                [return_maps]), State));

                [Partial] ->
                    State#{partial => Partial};

                [] ->
                    State#{partial => <<>>}
            end
    end.


register_docker(Id, Address) ->
    register_docker_a(Id, Address),
    register_docker_ptr(Id, Address).


register_docker_a(Id, Address) ->
    haystack_node:add(
      haystack_docker_util:docker_id(Id),
      in,
      a,
      haystack_docker_util:ttl(),
      Address).


register_docker_ptr(Id, {IP1, IP2, IP3, IP4}) ->
    haystack_node:add(
      haystack_name:labels(
        [integer_to_binary(IP4),
         integer_to_binary(IP3),
         integer_to_binary(IP2),
         integer_to_binary(IP1),
         <<"in-addr">>,
         <<"arpa">>]),
      in,
      ptr,
      #{name => haystack_docker_util:docker_id(Id)}).
