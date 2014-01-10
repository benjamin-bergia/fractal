-module(view_core).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

-record(state, {name, tid, dead_engine, dead_threshold, alive_engine, alive_threshold, suspicious_engine, suspicious_threshold}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, dead/3, alive/3, suspicious/3, handle_sync_event/4, terminate/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
	gen_fsm:start_link({local, ?SERVER}, ?MODULE, [Args], []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init({Tid, DE, DT, AE, AT, SE, ST}) ->
	S = #state{name=?MODULE,
		   tid=Tid,
		   dead_engine=DE,
		   dead_threshold=DT,
		   alive_engine=AE,
		   alive_threshold=AT,
		   suspicious_engine=SE,
		   suspicious_threshold=ST},
	{ok, dead, S}.

dead({dead_acc, DeadSum, AliveSum, SuspiciousSum}, _From, S) ->
	StateName = start_engine(dead, S#state.dead_engine, S#state.dead_threshold, DeadSum, AliveSum, SuspiciousSum),
	{reply, ok, StateName, S};
dead(_Event, _From, S) ->
	{reply, ok, dead, S}.

alive({alive_acc, DeadSum, AliveSum, SuspiciousSum}, _From, S) ->
	StateName = start_engine(alive, S#state.alive_engine, S#state.alive_threshold, DeadSum, AliveSum, SuspiciousSum),
	{reply, ok, StateName, S};
alive(_Event, _From, S) ->
	{reply, ok, alive, S}.

suspicious({suspicious_acc, DeadSum, AliveSum, SuspiciousSum}, _From, S) ->
	StateName = start_engine(alive, S#state.suspicious_engine, S#state.suspicious_threshold, DeadSum, AliveSum, SuspiciousSum),
	{reply, ok, StateName, S};
suspicious(_Event, _From, S) ->
	{reply, ok, suspicious, S}.

handle_sync_event(_Event, _From, StateName, State) ->
	{reply, ok, StateName, State}.

terminate(_Reason, _StateName, _State) ->
	ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start_engine(Status, Engine, Threshold, DeadSum, AliveSum, SuspiciousSum) ->
	{ok, StateName} = spawn(Engine, start, [Status, Threshold, DeadSum, AliveSum, SuspiciousSum]),
	StateName.

%% Include the unit tests 
-ifdef(TEST).
-include("test/view_core_tests.hrl").
-endif.
