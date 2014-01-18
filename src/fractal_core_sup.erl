
-module(fractal_core_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(VIEW(Name, Lowers), {Name, {view_sup, start_link, [Name, Lowers, weighted_engine, 1, weighted_engine, 1, weighted_engine, 1]}, permanent, 5000, supervisor, [view_sup]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Childs = [?VIEW("Host", [{"Memory", 1}, {"Cpu", 1}, {"Disk", 1}]),
		 ?VIEW("Memory", [{"test", 1}]),
		 ?VIEW("Cpu", [{"test", 1}]),
		 ?VIEW("Disk", [{"test", 1}])],
	{ok, {{one_for_one, 5, 10}, Childs}}.

