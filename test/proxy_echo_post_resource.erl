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

-module(proxy_echo_post_resource).

-export([init/2]).

init(Req0, State) ->
    case cowboy_req:body(Req0) of
        {ok, Body, Req1} ->
            Req2 = cowboy_req:reply(
                     200, [
                           {<<"content-type">>, <<"text/plain">>}
                          ], Body, Req1),
            {ok, Req2, State}
    end.
