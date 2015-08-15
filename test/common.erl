-module(common).
-include_lib("common_test/include/ct.hrl").
-export([
	 all/1,
	 consult/2,
	 read_file/2
	]).

is_a_test(is_a_test) ->
    false;
is_a_test(Function) ->
    hd(lists:reverse(string:tokens(atom_to_list(Function), "_"))) =:= "test".

all(Module) ->
    [Function || {Function, Arity} <- Module:module_info(exports),		
		 Arity =:= 1,
		 is_a_test(Function)].

consult(Config, Name) ->
    {ok, [Terms]} = file:consult(filename:join(?config(data_dir, Config), Name)),
    Terms.

read_file(Config, Name) ->
    {ok, Packet} = file:read_file(filename:join(?config(data_dir, Config), Name)),
    Packet.
