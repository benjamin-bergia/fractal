-module(view_rx).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {name, tid, subscriptions}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3, forward/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/3, handle_cast/2, terminate/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Name, Tid, Subscriptions) ->
	gen_server:start_link(?MODULE, [Name, Tid, Subscriptions], []).

forward(From, Status) ->
	Pids = gproc:lookup_pids({p, l, From}),
	Send = fun(Pid) ->
			gen_server:cast(Pid, {status_change, From, Status})
		end,
	lists:all(Send, Pids).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Name, Tid, Subscriptions) ->
	view_sup:set_pid(Tid, ?MODULE, Name, self()),
	subscribe(Subscriptions),	
	{ok, #state{name=Name, tid=Tid, subscriptions=Subscriptions}}.

handle_cast({status_change, View, Status}, S) ->
	view_acc:forward(S#state.name, S#state.tid, View, Status),
	{noreply, S}.

terminate(_Reason, _State) ->
	ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

subscribe(Names) ->
	Register = fun(Name, Acc) ->
			[{Name, sub}|Acc]
		end,
	Subscriptions = lists:foldl(Register, [], Names),
	gproc:mreg(p, l, Subscriptions).
