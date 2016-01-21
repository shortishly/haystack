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
-export([init/3]).
-export([terminate/3]).


init(_Transport, Req, #{prefix := Prefix, balancer := Balancer}) ->
    {Host, _} = cowboy_req:host(Req),
    case Balancer:pick(<<Prefix/binary, Host/binary>>) of
        not_found ->
            {ok, Req, undefined};

        #{host := Endpoint, port := Port} ->
            {ok, Origin} = gun:open(Endpoint, Port, #{transport => tcp}),
            {loop, Req, #{origin => Origin,
                          monitor => erlang:monitor(process, Origin)}}
    end.


info({gun_data, _, _, nofin, Data}, Req, State) ->
    case cowboy_req:chunk(Data, Req) of
        ok ->
            {loop, Req, State};

        {error, _} ->
            {ok, Req, State}
    end;

info({gun_data, _, _, fin, Data}, Req, State) ->
    cowboy_req:chunk(Data, Req),
    {ok, Req, State};

info({gun_response, _, _, nofin, Status, Headers}, Req1, State) ->
    {ok, Req2} = cowboy_req:chunked_reply(Status, Headers, Req1),
    {loop, Req2, State};

info({gun_response, _, _, fin, Status, Headers}, Req1, State) ->
    {ok, Req2} = cowboy_req:reply(Status, Headers, Req1),
    {ok, Req2, State};

info({gun_up, Origin, http}, Req1, #{origin := Origin} = State) ->
    {Path, Req2} = cowboy_req:path(Req1),
    {Headers, Req3} = cowboy_req:headers(Req2),
    {Method, Req4} =  cowboy_req:method(Req3),
    {loop,
     Req4,
     maybe_request_body(Req4,
                        State#{request => gun:request(Origin,
                                                      Method, Path, Headers)})};

info({request_body, #{complete := Data}}, Req, #{origin := Origin,
                                                 request := Request} = State) ->
    gun:data(Origin, Request, fin, Data),
    {loop, Req, State};

info({request_body, #{more := More}}, Req, #{origin := Origin,
                                             request := Request} = State) ->
    gun:data(Origin, Request, nofin, More),
    {loop, Req, request_body(Req, State)};

info({'DOWN', Monitor, _, _, _}, Req, #{monitor := Monitor} = State) ->
    {ok, Req, State}.


terminate(_Reason, _Req, #{origin := Origin}) ->
    gun:close(Origin).


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
