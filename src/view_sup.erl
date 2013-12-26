
-module(view_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(VIEW(N, E, W, U, L), {N, {view, start_link, [{N, {E, W}, dead, [?MODULE|U], L}]}, permanent, 5000, worker, [view]}).
%% Name / Engine / Weight / UpperViews / LowerViews 

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	ViewList = [?VIEW(a, onee_for_all, 1, [ab], [self()]),
		    ?VIEW(b, onee_for_all, 1, [ab], [self()]),
		    ?VIEW(ab, onee_for_all, 1, [abcd], [a, b]),
		    ?VIEW(c, onee_for_all, 1, [cd], [self()]),
		    ?VIEW(d, onee_for_all, 1, [cd], [self()]),
		    ?VIEW(cd, onee_for_all, 1, [abcd], [c, d]),
		    ?VIEW(abcd, onee_for_all, 1, [], [ab, cd])],
	{ok, {{one_for_one, 5, 10}, ViewList}}.

