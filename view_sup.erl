-module(view_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
	StateData = {first, {weighted, 1}, alive, [], []},
	FirstView = {firstview, {view_fsm, start_link, [StateData]}, permanent, 2000, worker,[view_fsm]},
	{ok, {{one_for_one,1 ,1},[FirstView]}}.


