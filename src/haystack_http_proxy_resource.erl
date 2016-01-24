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

-module(haystack_http_proxy_resource).


-export([info/3]).
-export([init/2]).
-export([terminate/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).


init(Req, #{balancer := Balancer} = State) when is_atom(Balancer) ->
    init(Req, State#{balancer := fun Balancer:pick/2});

init(Req0, #{prefix := Prefix, balancer := Balancer} = State) ->
    case Balancer(<<
                     Prefix/binary,
                     (cowboy_req:host(Req0))/binary
                   >>,
                   cowboy_req:path(Req0)) of

        not_found ->
            Req1 = cowboy_req:reply(404,
                                    [<<"content-type">>,
                                     <<"text/plain">>],
                                    "Not found.", Req0),
            {stop, Req1, undefined};

        #{host := Endpoint, port := Port, path := Path} ->
            {ok, Origin} = gun:open(Endpoint, Port, #{transport => tcp}),
            Monitor = erlang:monitor(process, Origin),

            case cowboy_req:parse_header(<<"upgrade">>, Req0) of
                undefined ->
                    {cowboy_loop,
                     Req0,
                     State#{origin => Origin,
                            monitor => Monitor,
                            path => Path}};

                [<<"websocket">>] ->
                    {cowboy_websocket,
                     Req0,
                     State#{origin => Origin,
                            monitor => Monitor,
                            path => Path}}
            end
    end.

info({gun_up, Origin, _}, Req0, #{path := Path, origin := Origin} = State) ->
    {ok,
     Req0,
     maybe_request_body(Req0,
                        State#{
                          request => gun:request(
                                       Origin,
                                       cowboy_req:method(Req0),
                                       Path,
                                       cowboy_req:headers(Req0))})};

info({gun_data, _, _, nofin, Data}, Req, State) ->
    case cowboy_req:chunk(Data, Req) of
        ok ->
            {ok, Req, State};

        {error, _} ->
            {stop, Req, State}
    end;

info({gun_data, _, _, fin, Data}, Req, State) ->
    cowboy_req:chunk(Data, Req),
    {stop, Req, State};

info({gun_response, _, _, nofin, Status, Headers}, Req0, State) ->
    Req1 = cowboy_req:chunked_reply(Status, Headers, Req0),
    {ok, Req1, State};

info({gun_response, _, _, fin, Status, Headers}, Req0, State) ->
    Req1 = cowboy_req:reply(Status, Headers, Req0),
    {stop, Req1, State};

info({request_body, #{complete := Data}}, Req, #{origin := Origin,
                                                 request := Request} = State) ->
    gun:data(Origin, Request, fin, Data),
    {ok, Req, State};

info({request_body, #{more := More}}, Req, #{origin := Origin,
                                             request := Request} = State) ->
    gun:data(Origin, Request, nofin, More),
    {ok, Req, request_body(Req, State)};

info({'DOWN', Monitor, _, _, _}, Req, #{monitor := Monitor} = State) ->
    {stop, Req, State}.


terminate(_Reason, _Req, #{origin := Origin, monitor := Monitor}) ->
    erlang:demonitor(Monitor),
    gun:close(Origin);
terminate(_Reason, _Req, _) ->
    ok.

websocket_info({'DOWN', Monitor, _, _, _},
               Req,
               #{monitor := Monitor} = State) ->
    {stop, Req, State};

websocket_info({gun_up, Origin, _},
               Req,
               #{path := Path, origin := Origin} = State) ->
    gun:ws_upgrade(Origin, Path),
    {ok, Req, State};

websocket_info({gun_ws_upgrade, Origin, ok, _},
               Req,
               #{origin := Origin,
                 ws_send_backlog := Frames} = State) ->
    gun:ws_send(Origin, lists:reverse(Frames)),
    {ok, Req, maps:without([ws_send_backlog], State#{ws_upgrade => handshake})};

websocket_info({gun_ws_upgrade, _, _,  _}, Req, State) ->
    {ok, Req, State#{ws_upgrade => handshake}};

websocket_info({gun_ws, Origin, Frame}, Req, #{origin := Origin} = State) ->
    {reply, Frame, Req, State};

websocket_info({gun_response, _, _, nofin, _, _}, Req, State) ->
    {ok, Req, State};

websocket_info({gun_response, _, _, fin, _, _}, Req, State) ->
    {stop, Req, State}.

websocket_handle(Frame, Req, #{origin := Origin,
                               ws_upgrade := handshake} = State) ->
    gun:ws_send(Origin, Frame),
    {ok, Req, State};

websocket_handle(Frame, Req, #{ws_send_backlog := Backlog} = State) ->
    {ok, Req, State#{ws_send_backlog := [Frame | Backlog]}};

websocket_handle(Frame, Req, State) ->
    {ok, Req, State#{ws_send_backlog => [Frame]}}.


maybe_request_body(Req, State) ->
    case cowboy_req:has_body(Req) of
        true ->
            request_body(Req, State);

        false ->
            State
    end.

request_body(Req, State) ->
    case cowboy_req:body(Req) of
        {ok, Data, _} ->
            self() ! {request_body, #{complete => Data}},
            State;

        {more, Data, _} ->
            self() ! {request_body, #{more => Data}}
    end.
