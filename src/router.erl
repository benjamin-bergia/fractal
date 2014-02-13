-module(router).
-define(STORE, store_rx).

%% ------------------------------------------------------------------
%% State record
%% ------------------------------------------------------------------

-record(view_status, {view_id, status}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([forward/2, query/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Forward a status change to the corresponding view
%% With:
%% 	To: Id of the view to forward to
%% 	dead/alive/suspicious: Status
%% @end
%%--------------------------------------------------------------------
forward(To, dead) ->
	forward_safe(To, dead);
forward(To, alive) ->
	forward_safe(To, alive);
forward(To, suspicious) ->
	forward_safe(To, suspicious).
forward_safe(To, Status) ->
	view:notify(To, Status).

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Send a message to the store to get the status of a given view
%% With:
%% 	ViewID: Id of the view to forward to
%% @end
%%--------------------------------------------------------------------
query(ViewID) ->
	?STORE:get_status(ViewID).
