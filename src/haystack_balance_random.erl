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

-module(haystack_balance_random).
-export([pick/2]).

pick(Hostname, Path) ->
    haystack_metric:increment(
      #{hostname => Hostname,
        module => ?MODULE,
        path => Path}),

    case dns_node:find(dns_name:labels(Hostname), in, srv) of
        not_found ->
            not_found;

        Matches ->
            random:seed(
              erlang:phash2(node()),
              erlang:monotonic_time(),
              erlang:unique_integer()),
            (pick_one_from(Matches))#{path => Path}
    end.

pick_one_from(#{data := #{target := Target, port := Port}}) ->
    [#{data := Address} | _] = dns_node:find(Target, in, a),
    #{host => inet:ntoa(Address), port => Port};
pick_one_from(Matches) ->
    pick_one_from(lists:nth(random:uniform(length(Matches)), Matches)).
