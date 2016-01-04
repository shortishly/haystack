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

-module(haystack_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    try
        {ok, _} = cowboy:start_http(?MODULE,
                                    100,
                                    [{port, haystack_config:port(http)}],
                                    [{env, [dispatch()]}]),
        {ok, Sup} = haystack_sup:start_link(),
        {ok, Sup, #{listeners => [?MODULE]}}
    catch
        _:Reason ->
            {error, Reason}
    end.

stop(#{listeners := Listeners}) ->
    lists:foreach(fun cowboy:stop_listener/1, Listeners);
stop(_State) ->
    ok.


dispatch() ->
    {dispatch, cowboy_router:compile([{'_', resources()}])}.

resources() ->
    [{"/zones", haystack_zone_resource, []},
     {"/secrets", haystack_secret_resource, []}].
