-module(dns).
-export([
	 start/0,
	 make/0,
	 get_env/1
	]).


start() ->
    application:ensure_all_started(?MODULE).

make() ->
    make:all([load]).

get_env(Key) ->
    gproc:get_env(l, ?MODULE, Key, [os_env, app_env]).
