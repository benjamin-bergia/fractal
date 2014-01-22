-module(engine_sup).
-behaviour(supervisor).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0]).

%% ------------------------------------------------------------------
%% Supervisor Function Exports
%% ------------------------------------------------------------------
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Start and link to a new engine supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Notify the View from a Status change (called by a lower view)
%% With:
%% 	From: name of calling view
%% 	Status: new status of the calling view
%% @end
%%--------------------------------------------------------------------
init([]) ->
	Engines = [{weighted_engine, {weighted_engine, start_link, []}, permanent, 5000, worker, [engine_sup]}],
	{ok, {{one_for_one, 5, 10}, Engines}}.
