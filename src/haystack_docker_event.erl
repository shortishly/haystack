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

-module(haystack_docker_event).
-export([process/2]).

process(#{<<"id">> := Id,
          <<"time">> := Time,
          <<"status">> := <<"stop">>},
        #{start_time := StartTime} = State) when Time > StartTime ->
    haystack_docker_container:remove(Id),
    State;

process(#{<<"id">> := Id,
          <<"time">> := Time,
          <<"status">> := <<"start">>},
        #{start_time := StartTime} = State) when Time > StartTime ->
    networks(State),
    inspect_container(Id, State),
    State;

process(_, State) ->
    State.


inspect_container(Id, State) ->
    case haystack_docker_util:request(["/containers/", Id, "/json"], State) of
        {ok, {{_, 200, _}, _, Body}} ->
            process_container(jsx:decode(Body, [return_maps]));

        {ok, {{_, 404, _}, _, _}} ->
            error_logger:info_report([{module, ?MODULE},
                                      {line, ?LINE},
                                      {reason, not_found},
                                      {id, Id}]);

        {error, Reason} ->
            error_logger:error_report([{module, ?MODULE},
                                       {line, ?LINE},
                                       {reason, Reason},
                                       {id, Id}])
    end.


process_container(#{<<"NetworkSettings">> :=  #{<<"Ports">> := null}}) ->
    nothing_to_register;
process_container(#{<<"Config">> := #{<<"Image">> := Image},
                    <<"Id">> := Id,
                    <<"Name">> := Name,
                    <<"NetworkSettings">> := #{<<"Ports">> := Ports}}) ->
    haystack_docker_container:process([#{<<"Image">> => Image,
                                         <<"Id">> => Id,
                                         <<"Names">> => [Name],
                                         <<"Ports">> => maps:to_list(Ports)}]).


networks(State) ->
    case haystack_docker_util:request(["/networks"], State) of
        {ok, {{_, 200, _}, _, Networks}} ->
            haystack_docker_network:process(Networks)
    end.
