-module(view_acc).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {name, deads=[], alives=[], suspiciouses=[]}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, terminate/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Name) ->
	S = #state{name=Name},
	gen_server:start_link({local, ?SERVER}, ?MODULE, S, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(S) ->
	{ok, S}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

terminate(_Reason, _State) ->
	ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

% message sent to FSM: {dead_acc, {alive, AliveSum}, {dead, DeadSum}, {suspicious, SuspiciousSum}}
