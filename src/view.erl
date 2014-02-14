-module(view).
-define(SUP, view_sup).
-define(RX, view_rx).
-define(STORE, store_rx).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/8, propagate/2, notify/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Start and link to a new view
%% With:
%% 	ViewID: the id of the new view
%% 	Lowers: a list of 2-Tuples containing the lower views and
%% 			weights associated to them
%% 	DE: the engine to use when dead
%% 	DT: the threshold to use when dead
%% 	AE: the engine to use when alive
%% 	AT: the threshold to use when alive
%% 	SE: the engine to use when suspicious
%% 	ST: the threshold to use when suspicious
%% @end
%%--------------------------------------------------------------------
start_link(ViewID, Lowers, DE, DT, AE, AT, SE, ST) ->
	?SUP:start_link(ViewID, Lowers, DE, DT, AE, AT, SE, ST).

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Notify the View from a Status change and record it in the store
%% 	(called by a lower view)
%% With:
%% 	From: id of calling view
%% 	Status: new status of the calling view
%% @end
%%--------------------------------------------------------------------
propagate(From, Status) ->
	?STORE:set_status(From, Status),
	?RX:forward(From, Status).

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Notify the View from a Status change (called by the router)
%% With:
%% 	From: id of calling view
%% 	Status: new status of the calling view
%% @end
%%--------------------------------------------------------------------
notify(From, Status) ->
	?RX:forward(From, Status).
