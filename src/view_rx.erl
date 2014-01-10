-module(view_rx).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {name, tid, accumulator, subscriptions}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, terminate/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({Name, Tid, Accumulator, Subscriptions}) ->
	S = #state{name=Name, tid=Tid, accumulator=Accumulator, subscriptions=Subscriptions},
	view_sup:set_pid(Tid, Name, self()),
	subscribe(Subscriptions),	
	{ok, S}.

handle_call({status_change, _View, _Status}=Msg, _From, S) ->
	Accumulator = view_sup:get_pid(S#state.tid, S#state.accumulator),
	forward(Accumulator, Msg),
	{reply, ok, S}.

terminate(_Reason, _State) ->
	ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

subscribe(Names) ->
	F = fun(Name, Acc) ->
			[{Name, sub}|Acc]
		end,
	Subscriptions = lists:foldl(F, [], Names),
	gproc:mreg(p, l, Subscriptions).

forward(To, Msg) ->
	gen_server:call(To, Msg).
