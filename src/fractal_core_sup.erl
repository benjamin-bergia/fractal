
-module(fractal_core_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Childs = [{view_sup_sup, {view_sup_sup, start_link, []}, permanent, 2000, supervisor, [view_sup_sup]},
		 {fractal_store_sup, {fractal_store_sup, start_link, []}, permanent, 2000, supervisor, [fractal_store_sup]}],
	{ok, {{one_for_one, 5, 10}, Childs}}.
