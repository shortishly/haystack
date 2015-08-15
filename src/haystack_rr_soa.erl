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

-module(haystack_rr_soa).
-export([decode/3]).
-export([encode/4]).

decode(_, Data, Packet) ->
    {MNAME, Remainder} = haystack_name:decode(Data, Packet),
    {RNAME, <<
              Serial:32,
              Refresh:32,
              Retry:32,
              Expire:32,
              Minimum:32
            >>} = haystack_name:decode(Remainder, Packet),
    #{m_name => MNAME,
      r_name => RNAME,
      serial => Serial,
      refresh => Refresh,
      retry => Retry,
      expire => Expire,
      minimum => Minimum}.

encode(#{m_name := MNAME,
         r_name := RNAME,
         serial := Serial,
         refresh := Refresh,
         retry := Retry,
         expire := Expire,
         minimum := Minimum}, _, Packet1, Offsets1) ->
    {Packet2, Offsets2} = haystack_name:encode(MNAME, Packet1, Offsets1),
    {Packet3, Offsets3} = haystack_name:encode(RNAME, Packet2, Offsets2),
    {<<
       Packet3/binary,
       Serial:32,
       Refresh:32,
       Retry:32,
       Expire:32,
       Minimum:32
     >>,
     Offsets3}.
