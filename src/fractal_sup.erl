-module(fractal_sup).

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
	Childs = [{tree_sup, {tree_sup, start_link, []}, permanent, 2000, supervisor, [tree_sup]},
		 {store_sup, {store_sup, start_link, []}, permanent, 2000, supervisor, [store_sup]}],
	{ok, {{one_for_one, 5, 10}, Childs}}.
