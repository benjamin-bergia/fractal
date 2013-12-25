-module(view_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
	StateData = {firstview, {one_for_all, 1}, dead, [?MODULE], []},
	FirstView = {firstview, {view_fsm, start_link, [firstview, StateData]}, permanent, 2000, worker,[view_fsm]},

	StateData2 = {secondview, {one_for_all, 1}, dead, [?MODULE], []},
	SecondView = {secondview, {view_fsm, start_link, [secondview, StateData2]}, permanent, 2000, worker,[view_fsm]},
	{ok, {{one_for_one,1 ,1},[FirstView, SecondView]}}.
