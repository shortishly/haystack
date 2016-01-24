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

-module(haystack_secret_resource).
-export([allow_missing_post/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([from_text_plain/2]).
-export([init/2]).


init(Req, _) ->
    {cowboy_rest, Req, undefined}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

allow_missing_post(Req, State) ->
    {false, Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"text">>, <<"plain">>, []}, from_text_plain}], Req, State}.

from_text_plain(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    case haystack_secret:process(Body) of
        ok ->
            {true, Req2, State};

        {error, #{line := _Line, message := Message}} ->
            {false,
             cowboy_req:set_resp_body(Message, Req2),
             State}
    end.
