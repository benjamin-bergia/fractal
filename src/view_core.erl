-module(view_core).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

-record(state, {dead_engine, dead_threshold, alive_engine, alive_threshold, suspicious_engine, suspicious_threshold}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, dead/3, alive/3, suspicious/3, handle_sync_event/4, terminate/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
	gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	{ok, initial_state_name, initial_state}.

dead({dead_acc, DeadSum, AliveSum, SuspiciousSum}, _From, S) ->
	Engine = S#state.dead_engine,
	Threshold = S#state.dead_threshold,
	StateName = start_engine(dead, Engine, Threshold, DeadSum, AliveSum, SuspiciousSum),
	{reply, ok, StateName, S};
dead(_Event, _From, S) ->
	{reply, ok, dead, S}.

alive({alive_acc, DeadSum, AliveSum, SuspiciousSum}, _From, S) ->
	Engine = S#state.alive_engine,
	Threshold = S#state.alive_threshold,
	StateName = start_engine(alive, Engine, Threshold, DeadSum, AliveSum, SuspiciousSum),
	{reply, ok, StateName, S};
alive(_Event, _From, S) ->
	{reply, ok, alive, S}.

suspicious({suspicious_acc, DeadSum, AliveSum, SuspiciousSum}, _From, S) ->
	Engine = S#state.suspicious_engine,
	Threshold = S#state.suspicious_threshold,
	StateName = start_engine(alive, Engine, Threshold, DeadSum, AliveSum, SuspiciousSum),
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
