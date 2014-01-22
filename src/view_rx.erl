-module(view_rx).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% State record
%% ------------------------------------------------------------------

-record(state, {name, tid, subscriptions}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3, forward/2]).

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
%%	Start and link a new receiver
%% With:
%% 	Name: the receiver's name
%% 	Tid: the supervisor ETS table Id
%% 	Subscriptions: a list of lower views
%% @end
%%--------------------------------------------------------------------
start_link(Name, Tid, Subscriptions) ->
	gen_server:start_link(?MODULE, {Name, Tid, Subscriptions}, []).

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Forward a message to all suscribers 
%% With:
%% 	From: the name of the publisher
%% 	Status: the current status name
%% @end
%%--------------------------------------------------------------------
forward(From, Status) ->
	Pids = gproc:lookup_pids({p, l, From}),
	Send = fun(Pid) ->
			gen_server:cast(Pid, {status_change, From, Status})
		end,
	lists:all(Send, Pids).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Register the process pid in the supervisor ETS table 
%% 	Suscribe to the lower views
%% 	Generate the initial state
%% With:
%% 	Name: the receiver's name
%% 	Tid: the supervisor ETS table Id
%% 	Subscriptions: a list of lower views
%% @end
%%--------------------------------------------------------------------
init({Name, Tid, Subscriptions}) ->
	view_sup:set_pid(Tid, ?MODULE, Name, self()),
	subscribe(Subscriptions),	
	{ok, #state{name=Name, tid=Tid, subscriptions=Subscriptions}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Handle lower view's notifications
%% With:
%% 	View: the name of the lower view
%% 	Status: the new status of the lower view
%% 	S: the current state of the receiver
%% @end
%%--------------------------------------------------------------------
handle_cast({status_change, View, Status}, S) ->
	view_acc:forward(S#state.name, S#state.tid, View, Status),
	{noreply, S}.

terminate(_Reason, _State) ->
	ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Suscribe to a list of lower views
%% With:
%% 	Names: a list of lower view's name	
%% @end
%%--------------------------------------------------------------------
subscribe(Names) ->
	Register = fun(Name, Acc) ->
			[{Name, sub}|Acc]
		end,
	Subscriptions = lists:foldl(Register, [], Names),
	gproc:mreg(p, l, Subscriptions).
