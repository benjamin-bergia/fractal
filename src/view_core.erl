-module(view_core).
-behaviour(gen_fsm).

%% ------------------------------------------------------------------
%% State record
%% ------------------------------------------------------------------

-record(state, {tid,
		dead_ngn, dead_thd,
		alive_ngn, alive_thd,
		suspicious_ngn, suspicious_thd}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/7, forward/5]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, dead/2, alive/2, suspicious/2, terminate/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Start and link a new core
%% With:
%% 	Tid: Supervisor ETS table Id
%% 	DE: Engine used when dead
%% 	DT: Threshold used when dead
%% 	AE: Engine used when alive
%% 	AT: Threshold used when alive
%% 	SE: Engine used when suspicious
%% 	ST: Threshold used when suspicious
%% @end
%%--------------------------------------------------------------------
start_link(Tid, DE, DT, AE, AT, SE, ST) ->
	gen_fsm:start_link(?MODULE, {Tid, DE, DT, AE, AT, SE, ST}, []).

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Forward a message to this module
%% With:
%% 	From: accumulator name
%% 	Tid: the supervisor ETS table Id
%% 	DSum: the sum of the dead views weights
%% 	ASum: the sum of the alive views weights
%% 	SSum: the sum of the suspicious views weights
%% @end
%%--------------------------------------------------------------------
forward(From, Tid, DSum, ASum, SSum) ->
	To = view_sup:get_pid(Tid, ?MODULE),
	Msg = {From, [{dead, DSum}, {alive, ASum}, {suspicious, SSum}]},
	gen_fsm:send_event(To, Msg).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Register process pid in the supervisor ETS Table
%%	Initialize the State
%% With:
%% 	Tid: Supervisor ETS table Id
%% 	DE: Engine used when dead
%% 	DT: Threshold used when dead
%% 	AE: Engine used when alive
%% 	AT: Threshold used when alive
%% 	SE: Engine used when suspicious
%% 	ST: Threshold used when suspicious
%% @end
%%--------------------------------------------------------------------
init({Tid, DE, DT, AE, AT, SE, ST}) ->
	view_sup:set_pid(Tid, ?MODULE, self()),
	S = #state{tid=Tid,
		   dead_ngn=DE, dead_thd=DT,
		   alive_ngn=AE, alive_thd=AT,
		   suspicious_ngn=SE, suspicious_thd=ST},
	{ok, dead, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Handle the asynchronous events when dead and coming
%% 		from the dead accumulator only
%% 	Get a new Status from the engine
%% 	Forward the new status to view_tx
%% With:
%% 	Dead: {dead, Sum} with Weights Sum of all the dead views
%% 	Alive: {alive, Sum} with Weights Sum of all the alive views
%% 	Suspicious: {suspicious, Sum} with Weights Sum of all the 
%% 					suspicious views
%% @end
%%--------------------------------------------------------------------
dead({dead, Data}, S) ->
	Status = engine:process(S#state.dead_ngn, dead, S#state.dead_thd, Data),
	view_tx:forward(S#state.tid, Status),
	{next_state, Status, S};
dead(_Event, S) ->
	{next_state, dead, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Handle the asynchronous events when alive and coming
%% 		from the alive accumulator only
%% 	Get a new Status from the engine
%% 	Forward the new status to view_tx
%% With:
%% 	Dead: {dead, Sum} with Weights Sum of all the dead views
%% 	Alive: {alive, Sum} with Weights Sum of all the alive views
%% 	Suspicious: {suspicious, Sum} with Weights Sum of all the 
%% 					suspicious views
%% @end
%%--------------------------------------------------------------------
alive({alive, Data}, S) ->
	Status = engine:process(S#state.alive_ngn, alive, S#state.alive_thd, Data),
	view_tx:forward(S#state.tid, Status),
	{next_state, Status, S};
alive(_Event, S) ->
	{next_state, alive, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Handle the asynchronous events when suspicious and coming
%% 		from the suspicious accumulator only
%% 	Get a new Status from the engine
%% 	Forward the new status to view_tx
%% With:
%% 	Dead: {dead, Sum} with Weights Sum of all the dead views
%% 	Alive: {alive, Sum} with Weights Sum of all the alive views
%% 	Suspicious: {suspicious, Sum} with Weights Sum of all the 
%% 					suspicious views
%% @end
%%--------------------------------------------------------------------
suspicious({suspicious, Data}, S) ->
	Status = engine:process(S#state.suspicious_ngn, suspicious, S#state.suspicious_thd, Data),
	view_tx:forward(S#state.tid, Status),
	{next_state, Status, S};
suspicious(_Event, S) ->
	{next_state, suspicious, S}.

terminate(_Reason, _Status, _State) ->
	ok.

%% Include the unit tests 
-ifdef(TEST).
-include("test/view_core_tests.hrl").
-endif.
