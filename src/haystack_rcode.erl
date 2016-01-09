%% Copyright (c) 2012-2015 Peter Morgan <peter.james.morgan@gmail.com>
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

-module(haystack_rcode).
-export([lookup/1]).

-on_load(on_load/0).


on_load() ->
    haystack_table:new(?MODULE),
    add(0, no_error),
    add(1, format_error),
    add(2, server_failure),
    add(3, name_error),
    add(4, not_implemented),
    add(5, refused),
    add(6, yx_domain),
    add(7, yx_rrset),
    add(8, nx_rrset),
    add(9, not_auth),
    add(10, not_zone),
    add(16, bad_sig),
    add(17, bad_key),
    add(18, bad_time).

-record(?MODULE, {
           rcode :: integer(),
           name :: atom()
          }).

add(RCODE, Name) ->
    ets:insert_new(?MODULE, [r(RCODE, Name)]) orelse
        error({badarg, [RCODE, Name]}),
    ok.

lookup(RCODE) when is_integer(RCODE) ->
    case ets:lookup(?MODULE, RCODE) of
        [#?MODULE{name = Name}] ->
            Name;

        [] ->
            error({badarg, RCODE})
    end;
lookup(Name) ->
    case ets:match_object(?MODULE, r('_', Name)) of
        [#?MODULE{rcode = RCODE}] ->
            RCODE;

        [] ->
            error({badarg, Name})
    end.

r(RCODE, Name) ->
    #?MODULE{rcode = RCODE, name = Name}.
