-module(view_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("state.hrl").
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(VIEW(S), {S#state.view_name, {view, start_link, [S]}, permanent, 5000, worker, [view]}).
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
	State = #state{engine=one_for_all, threshold=1, state_name=dead},
	ViewList = [?VIEW(State#state{view_name=view000, lower_views=[?LOWERS(view01), ?LOWERS(view02)]}),
		    ?VIEW(State#state{view_name=view010, upper_views=[view0], lower_views=[?LOWERS(view011), ?LOWERS(view012)]}),
		    ?VIEW(State#state{view_name=view020, upper_views=[view0], lower_views=[?LOWERS(view021), ?LOWERS(view022)]}),
		    ?VIEW(State#state{view_name=view011, upper_views=[view01], lower_views=[?LOWERS(view_sup)]}),
		    ?VIEW(State#state{view_name=view012, upper_views=[view01], lower_views=[?LOWERS(view_sup)]}),
		    ?VIEW(State#state{view_name=view021, upper_views=[view02], lower_views=[?LOWERS(view_sup)]}),
		    ?VIEW(State#state{view_name=view022, upper_views=[view02], lower_views=[?LOWERS(view_sup)]})],
	{ok, {{one_for_one, 5, 10}, ViewList}}.

