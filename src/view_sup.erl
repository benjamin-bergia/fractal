
-module(view_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(VIEW(N, S), {N, {view, start_link, [S]}, permanent, 5000, worker, [view]}).
%% Name / Engine / Weight / UpperViews / LowerViews 
-define(LOWERS(L), {L, dead, 1}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	ViewList = [?VIEW(view0, view:create_state({view0, one_for_all, 1, dead, [], [?LOWERS(view01), ?LOWERS(view02)]})),
		    ?VIEW(view01, view:create_state({view01, one_for_all, 1, dead, [view0], [?LOWERS(view011), ?LOWERS(view012)]})),
		    ?VIEW(view02, view:create_state({view02, one_for_all, 1, dead, [view0], [?LOWERS(view021), ?LOWERS(view022)]})),
		    ?VIEW(view011, view:create_state({view011, one_for_all, 1, dead, [view01], [?LOWERS(view_sup)]})),
		    ?VIEW(view012, view:create_state({view012, one_for_all, 1, dead, [view01], [?LOWERS(view_sup)]})),
		    ?VIEW(view021, view:create_state({view021, one_for_all, 1, dead, [view02], [?LOWERS(view_sup)]})),
		    ?VIEW(view022, view:create_state({view022, one_for_all, 1, dead, [view02], [?LOWERS(view_sup)]}))],
	{ok, {{one_for_one, 5, 10}, ViewList}}.

