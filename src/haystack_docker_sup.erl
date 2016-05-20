%% Copyright (c) 2012-2016 Peter Morgan <peter.james.morgan@gmail.com>
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

-module(haystack_docker_sup).
-behaviour(supervisor).

-export([init/1]).
-export([start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    case connection() of
        {ok, #{host := Host,
               port := Port,
               cert := Cert,
               key := Key}} ->
            {ok,
             {sup_flags(),
              [haystack_sup:worker(haystack_docker, transient, [Host, Port, Cert, Key])]}};
        
        {ok, Connections} when is_list(Connections) ->
            {ok,
             {sup_flags(),
              lists:map(
                fun
                    (#{host := Host, port := Port} = Connection) ->
                        haystack_sup:worker({haystack_docker, Connection}, haystack_docker, transient, [Host, Port])
                end,
                Connections)}};
        
        {error, _} = Error ->
            Error
    end.

sup_flags() ->
    #{intensity => 5, period => 5}.

connection() ->
    case {haystack_config:docker(host),
          haystack_config:docker(cert_path),
          haystack_config:docker(cert),
          haystack_config:docker(key)} of

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


connection(URIs) ->
    lists:foldl(
      fun
          (_, {error, _} = A) ->
              A;

          (URI, {ok, A}) ->
              case http_uri:parse(URI) of
                  {ok, {_, _, Host, Port, _, _}} ->
                      {ok, [#{host => Host, port => Port} | A]};

                  {error, _} = Error ->
                      Error
              end
      end,
      {ok, []},
      string:tokens(URIs, ",")).


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


read_file(Path, File) ->
    file:read_file(filename:join(Path, File)).

