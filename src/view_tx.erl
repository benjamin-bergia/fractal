-module(view_tx).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% State record
%% ------------------------------------------------------------------

-record(state, {name, tid, view_name}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2, forward/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_cast/2, terminate/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Start and link to a new transmitter
%% With
%% 	Tid: the supervisor ets table id
%% 	Viewname: the name of thee view containing this transmitter
%% @end
%%--------------------------------------------------------------------
start_link(Tid, ViewName) ->
	gen_server:start_link(?MODULE, {Tid, ViewName}, []).

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Forward message to this transmitter
%% With:
%% 	Tid: the supervisor ETS table Id
%% 	Status: the new status name to forward
%% @end
%%--------------------------------------------------------------------
forward(Tid, Status) ->
	TX = view_sup:get_pid(Tid, ?MODULE),
	gen_server:cast(TX, {status_change, Status}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Register the process pid in the supervisor ETS table
%% 	Initialize the state of the transmitter
%% With:
%% 	Tid: the supervisor ets table id
%% 	Viewname: the name of thee view containing this transmitter
%% @end
%%--------------------------------------------------------------------
init({Tid, ViewName}) ->
	view_sup:set_pid(Tid, ?MODULE, self()),
	{ok, #state{name=?MODULE, tid=Tid, view_name=ViewName}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Handle the status updates forwarded by the core
%% With:
%% 	Status: the new status of the core
%% 	S: the current state
%% @end
%%--------------------------------------------------------------------
handle_cast({status_change, Status}, S) ->
	view:notify(S#state.view_name, Status),
	{noreply, S}.

terminate(_Reason, _State) ->
	ok.
