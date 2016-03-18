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

-module(haystack_inet).
-export([cidr/1]).
-export([getifaddrs/1]).
-export([mask_bits/1]).
-export([network/1]).

getifaddrs(v4) ->
    {ok, Interfaces} = inet:getifaddrs(),

    lists:foldl(
      fun
          ({_, Properties}, A) ->
              [#{address => Addr, mask => mask_bits(Mask)} ||
                  {{_, _, _, _} = Addr, {_, _, _, _} = Mask} <-
                      lists:zip(
                        proplists:get_all_values(addr, Properties),
                        proplists:get_all_values(netmask, Properties)),
                  Addr /= {127, 0, 0, 1}] ++ A
      end,
      [],
      Interfaces).

cidr(AddressAndMask) ->
    case binary:split(AddressAndMask, <<"/">>) of
        [Address, Mask] ->
            {ok, IP} = inet:parse_ipv4_address(binary_to_list(Address)),
            #{address => IP, mask => binary_to_integer(Mask)};

        [Address] ->
            {ok, IP} = inet:parse_ipv4_address(binary_to_list(Address)),
            #{address => IP}
    end.

network(#{address := {IP1, IP2, IP3, IP4}, mask := Mask}) ->
    <<Network:Mask, _/bitstring>> = <<IP1, IP2, IP3, IP4>>,
    <<Net1:8, Net2:8, Net3:8, Net4:8>> = <<Network:Mask, 0:(32-Mask)>>,
    #{address => {Net1, Net2, Net3, Net4}, mask => Mask}.

mask_bits({IP1, IP2, IP3, IP4}) ->
    mask_bits(<<IP1, IP2, IP3, IP4>>, 0).

mask_bits(<<1:1, Remainder/bitstring>>, A) ->
    mask_bits(Remainder, A+1);
mask_bits(<<0:1, _/bitstring>>, A) ->
    A.
