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

-module(haystack_date).

-export([epoch/0]).
-export([right_now/0]).
-export([seconds_elapsed_since/1]).
-export([seconds_since_epoch/1]).
-export([to_datetime/1]).


epoch() ->
    calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).

right_now() ->
    seconds_since_epoch(erlang:universaltime()).

seconds_elapsed_since(DateTime) ->
    right_now() - seconds_since_epoch(DateTime).

seconds_since_epoch(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - epoch().

to_datetime(SecondsSinceEpoch) ->
    calendar:gregorian_seconds_to_datetime(epoch() +
                                               SecondsSinceEpoch).
