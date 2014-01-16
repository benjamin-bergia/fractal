
-module(fractal_core_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(N, I, Args, Type), {N, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Childs = [?CHILD("Host", view_sup, ["Host", [{"Memory", 1}, {"Cpu", 1}, {"Disk", 1}]], supervisor),
		 ?CHILD("Memory", view_sup, ["Memory", [{"test", 1}]], supervisor),
		 ?CHILD("Cpu", view_sup, ["Cpu", [{"test", 1}]], supervisor),
		 ?CHILD("Disk", view_sup, ["Disk", [{"test", 1}]], supervisor)],
	{ok, {{one_for_one, 5, 10}, Childs}}.

