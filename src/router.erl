-module(router).
-define(STORE, store_rx).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([notify/2, query/1, gen_pub/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Notify a status change to the corresponding view
%% With:
%% 	To: Id of the view to notify to
%% 	dead/alive/suspicious: Status
%% @end
%%--------------------------------------------------------------------
notify(To, dead) ->
	notify_safe(To, dead);
notify(To, alive) ->
	notify_safe(To, alive);
notify(To, suspicious) ->
	notify_safe(To, suspicious).
notify_safe(To, Status) ->
	view:notify(gen_pub(To), Status).

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Send a message to the store to get the status of a given view
%% With:
%% 	ViewID: Id of the view to notify to
%% @end
%%--------------------------------------------------------------------
query(ViewID) ->
	?STORE:get_status(ViewID).

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Generate new provider id allowing the view to subscribe to the router
%% With:
%% 	ViewID: Id of the view
%% @end
%%--------------------------------------------------------------------
gen_pub(ViewID) ->
	{router, ViewID}.
