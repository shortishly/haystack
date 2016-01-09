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


-module(haystack_header_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() ->
        [{group, tests}].

groups() ->
    [{tests, [parallel], common:all(?MODULE)}].


init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(haystack),
    Config.

end_per_suite(_Config) ->
    common:purge_application(haystack).

class_in_test(_Config) ->
    1 = haystack_class:lookup(in),
    in = haystack_class:lookup(1).

class_cs_test(_Config) ->
    2 = haystack_class:lookup(cs),
    cs = haystack_class:lookup(2).

class_ch_test(_Config) ->
    3 = haystack_class:lookup(ch),
    ch = haystack_class:lookup(3).

class_hs_test(_Config) ->
    4 = haystack_class:lookup(hs),
    hs = haystack_class:lookup(4).

class_any_test(_Config) ->
    255 = haystack_class:lookup(any),
    any = haystack_class:lookup(255).

type_a_test(_Config) ->
    1 = haystack_rr:lookup(a),
    a = haystack_rr:lookup(1).

type_ns_test(_Config) ->
    2 = haystack_rr:lookup(ns),
    ns = haystack_rr:lookup(2).

type_cname_test(_Config) ->
    5 = haystack_rr:lookup(cname),
    cname = haystack_rr:lookup(5).

type_soa_test(_Config) ->
    6 = haystack_rr:lookup(soa),
    soa = haystack_rr:lookup(6).

type_ptr_test(_Config) ->
    12 = haystack_rr:lookup(ptr),
    ptr = haystack_rr:lookup(12).

type_mx_test(_Config) ->
    15 = haystack_rr:lookup(mx),
    mx = haystack_rr:lookup(15).

type_txt_test(_Config) ->
    16 = haystack_rr:lookup(txt),
    txt = haystack_rr:lookup(16).

type_aaaa_test(_Config) ->
    28 = haystack_rr:lookup(aaaa),
    aaaa = haystack_rr:lookup(28).

type_srv_test(_Config) ->
    33 = haystack_rr:lookup(srv),
    srv = haystack_rr:lookup(33).

type_opt_test(_Config) ->
    41 = haystack_rr:lookup(opt),
    opt = haystack_rr:lookup(41).

type_tsig_test(_Config) ->
    250 = haystack_rr:lookup(tsig),
    tsig = haystack_rr:lookup(250).

type_any_test(_Config) ->
    255 = haystack_rr:lookup(any),
    any = haystack_rr:lookup(255).

opcode_query_test(_Config)->
    query = haystack_protocol:lookup(0),
    0 = haystack_protocol:lookup(query).

opcode_update_test(_Config)->
    update = haystack_protocol:lookup(5),
    5 = haystack_protocol:lookup(update).

ocode_query_qr_test(_Config) ->
    query = haystack_query:header(qr, 0),
    0 = haystack_query:header(qr, query),
    response = haystack_query:header(qr, 1),
    1 = haystack_query:header(qr, response).

ocode_update_qr_test(_Config) ->
    query = haystack_query:header(qr, 0),
    0 = haystack_query:header(qr, query),
    response = haystack_query:header(qr, 1),
    1 = haystack_query:header(qr, response).

ocode_query_aa_test(_Config) ->
    false = haystack_query:header(aa, 0),
    0 = haystack_query:header(aa, false),
    true = haystack_query:header(aa, 1),
    1 = haystack_query:header(aa, true).

ocode_query_tc_test(_Config) ->
    false = haystack_query:header(tc, 0),
    0 = haystack_query:header(tc, false),
    true = haystack_query:header(tc, 1),
    1 = haystack_query:header(tc, true).

ocode_query_rd_test(_Config) ->
    false = haystack_query:header(rd, 0),
    0 = haystack_query:header(rd, false),
    true = haystack_query:header(rd, 1),
    1 = haystack_query:header(rd, true).

ocode_query_ra_test(_Config) ->
    false = haystack_query:header(ra, 0),
    0 = haystack_query:header(ra, false),
    true = haystack_query:header(ra, 1),
    1 = haystack_query:header(ra, true).

rcode_test(_Config) ->
    no_error = haystack_rcode:lookup(0),
    0 = haystack_rcode:lookup(no_error),
    format_error = haystack_rcode:lookup(1),
    1 = haystack_rcode:lookup(format_error),
    server_failure = haystack_rcode:lookup(2),
    2 = haystack_rcode:lookup(server_failure),
    name_error = haystack_rcode:lookup(3),
    3 = haystack_rcode:lookup(name_error),
    not_implemented = haystack_rcode:lookup(4),
    4 = haystack_rcode:lookup(not_implemented),
    refused = haystack_rcode:lookup(5),
    5 = haystack_rcode:lookup(refused),
    yx_domain = haystack_rcode:lookup(6),
    6 = haystack_rcode:lookup(yx_domain),
    yx_rrset = haystack_rcode:lookup(7),
    7 = haystack_rcode:lookup(yx_rrset),
    nx_rrset = haystack_rcode:lookup(8),
    8 = haystack_rcode:lookup(nx_rrset),
    not_auth = haystack_rcode:lookup(9),
    9 = haystack_rcode:lookup(not_auth),
    not_zone = haystack_rcode:lookup(10),
    10 = haystack_rcode:lookup(not_zone).
