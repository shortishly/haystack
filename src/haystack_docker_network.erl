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

-module(haystack_docker_network).
-export([process/1]).

process(Networks) ->
    lists:foldl(fun
                    (#{<<"Containers">> := Containers}, A) ->
                        maps:merge(maps:fold(fun process_network_container/3,
                                             #{},
                                             Containers),
                                   A)
                end,
                #{},
                jsx:decode(Networks, [return_maps])).

process_network_container(Container,
                          #{<<"IPv4Address">> := AddressWithMask}, A) ->
    [Address, _Mask] = binary:split(AddressWithMask, <<"/">>),
    case inet:parse_address(binary_to_list(Address)) of
        {ok, IP} ->
            haystack_docker_container:add(Container, IP),
            A#{Container => IP};

        {error, _} ->
            A
    end.
