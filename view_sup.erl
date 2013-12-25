-module(view_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([update_firstview/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
	StateData = {first, {weighted, 1}, dead, [{local, secondview}], [{view_sup, dead, 1}]},
	FirstView = {firstview, {view_fsm, start_link, [{local, firstview}, StateData]}, permanent, 2000, worker,[view_fsm]},
	StateData2 = {second, {weighted, 1}, dead, [], [{{local, firstview}, dead, 1}]},
	SecondView = {secondview, {view_fsm, start_link, [{local, secondview}, StateData2]}, permanent, 2000, worker,[view_fsm]},
	{ok, {{one_for_one,1 ,1},[FirstView, SecondView]}}.

update_firstview(State) ->
	gen_fsm:send_event({local, firstview}, {view_sup, State}).
