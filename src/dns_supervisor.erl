-module(dns_supervisor).
-behaviour(supervisor).

-export([
	 start_link/0,
	 init/1
	]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [worker(dns_server)],
	{ok, {{one_for_one, 1, 5}, Procs}}.

worker(Module) ->
    worker(Module, permanent).

worker(Module, Restart) ->
    worker(Module, Restart, []).

worker(Module, Restart, Parameters) ->
    worker(Module, Module, Restart, Parameters).    

worker(Id, Module, Restart, Parameters) ->
    {Id, {Module, start_link, Parameters}, Restart, 5000, worker, [Module]}.
