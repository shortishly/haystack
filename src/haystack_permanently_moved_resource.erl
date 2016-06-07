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

-module(haystack_permanently_moved_resource).
-export([init/2]).
-export([moved_permanently/2]).
-export([previously_existed/2]).
-export([resource_exists/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

resource_exists(Req, State) ->
    {false, Req, State}.

previously_existed(Req, #{path := _} = State) ->
    {true, Req, State};
previously_existed(Req, State) ->
    {false, Req, State}.


moved_permanently(Req, #{path := Path} = State) ->
    {{true, <<
              (cowboy_req:host_url(Req))/bytes,
              Path/bytes
            >>}, Req, State};
moved_permanently(Req, State) ->
    {false, Req, State}.

