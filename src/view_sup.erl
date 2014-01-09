-module(view_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	%ViewList = config_parser:get_config("test.conf"),
	ViewList = [{view, {view, start_link, [{test, direct, 1, []}]}, permanent, 5000, worker, [view]}],
	{ok, {{one_for_one, 5, 10}, ViewList}}.

