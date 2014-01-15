-module(view_core).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

-record(state, {name, tid,
		dead_engine, dead_threshold,
		alive_engine, alive_threshold,
		suspicious_engine, suspicious_threshold}).

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

forward(To, From, DeadSum, AliveSum, SuspiciousSum) ->
	Msg = {From, {dead, DeadSum}, {alive, AliveSum}, {suspicious, SuspiciousSum}},
	gen_fsm:sync_send_event(To, From, Msg).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init({Tid, {DE, DT}, {AE, AT}, {SE, ST}}) ->
	S = #state{name=?MODULE,
		   tid=Tid,
		   dead_engine=DE,
		   dead_threshold=DT,
		   alive_engine=AE,
		   alive_threshold=AT,
		   suspicious_engine=SE,
		   suspicious_threshold=ST},
	view_sup:set_pid(Tid, ?MODULE, self()),
	{ok, dead, S}.

dead({dead_acc, DList, AList, SList}, _From, S) ->
	StateName = start_engine(dead, S#state.dead_engine, S#state.dead_threshold, DList, AList, SList),
	{reply, ok, StateName, S};
dead(_Event, _From, S) ->
	{reply, ok, dead, S}.

alive({alive_acc, DList, AList, SList}, _From, S) ->
	StateName = start_engine(alive, S#state.alive_engine, S#state.alive_threshold, DList, AList, SList),
	{reply, ok, StateName, S};
alive(_Event, _From, S) ->
	{reply, ok, alive, S}.

suspicious({suspicious_acc, DList, AList, SList}, _From, S) ->
	StateName = start_engine(alive, S#state.suspicious_engine, S#state.suspicious_threshold, DList, AList, SList),
	{reply, ok, StateName, S};
suspicious(_Event, _From, S) ->
	{reply, ok, suspicious, S}.

terminate(_Reason, _StateName, _State) ->
	ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start_engine(Status, Engine, Threshold, DSum, ASum, SSum) ->
	spawn(Engine, start, [Status, Threshold, DSum, ASum, SSum]).

%% Include the unit tests 
-ifdef(TEST).
-include("test/view_core_tests.hrl").
-endif.
