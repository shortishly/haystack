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

-module(haystack_permanently_moved_resource).
-export([init/3]).
-export([moved_permanently/2]).
-export([previously_existed/2]).
-export([resource_exists/2]).
-export([rest_init/2]).


init(_, _, _) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
    {Host, _} = cowboy_req:host(Req),
    case haystack_node:find(labels(Host), in, srv) of
        not_found ->
            {ok, Req, undefined};

        Matches ->
            random:seed(erlang:phash2(node()),
                        erlang:monotonic_time(),
                        erlang:unique_integer()),
            {ok, Req, #{host => pick_one_from(Matches)}}
    end.

labels(Host) ->
    prefix() ++ binary:split(Host, <<".">>, [global]).

prefix() ->
    [<<"_http">>, <<"_tcp">>].

resource_exists(Req, State) ->
    {false, Req, State}.


previously_existed(Req, #{host := _} = State) ->
    {true, Req, State};
previously_existed(Req, State) ->
    {false, Req, State}.


moved_permanently(Req, #{host := Host} = State) ->
    {Path, _} = cowboy_req:path(Req),
    {{true, <<"http://", Host/binary, Path/binary>>}, Req, State};
moved_permanently(Req, State) ->
    {false, Req, State}.


pick_one_from(#{data := #{target := Labels, port := Port}}) ->
    <<(join_the_dots(Labels))/binary, ":", (integer_to_binary(Port))/binary>>;
pick_one_from(Matches) ->
    pick_one_from(lists:nth(random:uniform(length(Matches)), Matches)).



join_the_dots([H1, H2 | T]) ->
    <<H1/binary, ".", (join_the_dots([H2 | T]))/binary>>;
join_the_dots([TLD]) ->
    <<TLD/binary>>.
