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

-module(haystack_rr_aaaa).
-export([decode/3]).
-export([encode/4]).

decode(_, <<IP1:16,
            IP2:16,
            IP3:16,
            IP4:16,
            IP5:16,
            IP6:16,
            IP7:16,
            IP8:16>>, _) ->
    {IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8}.


encode({IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8}, _, Packet, Offsets) ->
    {<<
       Packet/bytes,
       IP1:16,
       IP2:16,
       IP3:16,
       IP4:16,
       IP5:16,
       IP6:16,
       IP7:16,
       IP8:16
     >>, Offsets}.
