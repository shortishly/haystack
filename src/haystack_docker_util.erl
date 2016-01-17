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

-module(haystack_docker_util).
-export([container_id/1]).
-export([docker_id/1]).
-export([request/2]).
-export([ssl/2]).
-export([system_time/1]).
-export([ttl/0]).


request(Suffix, #{host := Host,
                  port := Port,
                  cert := Cert,
                  key := Key}) ->
    httpc:request(get,
                  {iolist_to_list(["https://",
                                   Host,
                                   ":",
                                   integer_to_list(Port),
                                   Suffix]), []},
                  [{ssl, ssl(Cert, Key)}],
                  [{body_format, binary}]);

request(Suffix, #{host := Host, port := Port}) ->
    httpc:request(get,
                  {iolist_to_list(["http://",
                                   Host,
                                   ":",
                                   integer_to_list(Port),
                                   Suffix]), []},
                  [],
                  [{body_format, binary}]).


iolist_to_list(IO) ->
    binary_to_list(iolist_to_binary(IO)).

ssl(Cert, Key) ->
    [{cert, Cert},
     {key, Key}].



ttl() ->
    100.


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
              _Remainder/binary>>) ->
    {{binary_to_integer(Year),
      binary_to_integer(Month),
      binary_to_integer(Date)},
     {binary_to_integer(Hour),
      binary_to_integer(Minute),
      binary_to_integer(Second)}}.


docker_id(<<_:59/bytes>> = Id) ->
    id(<<"d">>, Id, dockers).


container_id(<<_:64/bytes>> = Id) ->
    id(<<"c">>, Id, containers).


id(Prefix, Id, Origin) ->
    haystack_name:labels(<<
                           Prefix/binary,
                           (hash(Id))/binary,
                           ".",
                           (haystack_config:origin(Origin))/binary
                         >>).


hash(Name) ->
    list_to_binary(
      string:to_lower(
        integer_to_list(erlang:phash2(Name), 26))).
