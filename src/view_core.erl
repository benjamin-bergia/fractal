-module(view_core).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

-record(state, {name, tid,
		dead_ngn, dead_thd,
		alive_ngn, alive_thd,
		suspicious_ngn, suspicious_thd}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, forward/5]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, dead/3, alive/3, suspicious/3, terminate/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
	gen_fsm:start_link(?MODULE, Args, []).

forward(From, Tid, DSum, ASum, SSum) ->
	To = view_sup:get_pid(Tid, ?MODULE),
	Msg = {From, {dead, DSum}, {alive, ASum}, {suspicious, SSum}},
	gen_fsm:sync_send_event(To, From, Msg).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init({Tid, {DE, DT}, {AE, AT}, {SE, ST}}) ->
	view_sup:set_pid(Tid, ?MODULE, self()),
	S = #state{name=?MODULE, tid=Tid,
		   dead_ngn=DE, dead_thd=DT,
		   alive_ngn=AE, alive_thd=AT,
		   suspicious_ngn=SE, suspicious_thd=ST},
	{ok, dead, S}.

dead({dead, Dead, Alive, Suspicious}, _From, S) ->
	{stop, Status} = start_engine(dead, S#state.dead_ngn, S#state.dead_thd, [Dead, Alive, Suspicious]),
	view_tx:forward(S#state.tid, ?MODULE, Status),
	{reply, ok, Status, S};
dead(_Event, _From, S) ->
	{reply, ok, dead, S}.

alive({alive, Dead, Alive, Suspicious}, _From, S) ->
	{stop, Status} = start_engine(alive, S#state.alive_ngn, S#state.alive_thd, [Dead, Alive, Suspicious]),
	view_tx:forward(S#state.tid, view_core, Status),
	{reply, ok, Status, S};
alive(_Event, _From, S) ->
	{reply, ok, alive, S}.

suspicious({suspicious, Dead, Alive, Suspicious}, _From, S) ->
	{stop, Status} = start_engine(alive, S#state.suspicious_ngn, S#state.suspicious_thd, [Dead, Alive, Suspicious]),
	view_tx:forward(S#state.tid, view_core, Status),
	{reply, ok, Status, S};
suspicious(_Event, _From, S) ->
	{reply, ok, suspicious, S}.

terminate(_Reason, _Status, _State) ->
	ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start_engine(Status, Engine, Threshold, StatusList) ->
	spawn(Engine, start, [Status, Threshold, StatusList]).

%% Include the unit tests 
-ifdef(TEST).
-include("test/view_core_tests.hrl").
-endif.
