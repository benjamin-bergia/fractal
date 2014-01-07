-module(view_fsm).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, alive/3, dead/3, suspicious/3, terminate/3]).

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

alive({alive_acc, _, _}, _From, State) ->
	{reply, ok, state_name, State}.
alive(_Event, _From, State) ->
	{reply, ok, alive, State}.

dead(_Event, _From, State) ->
	{reply, ok, state_name, State}.

suspicious(_Event, _From, State) ->
	{reply, ok, state_name, State}.

terminate(_Reason, _StateName, _State) ->
	ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


