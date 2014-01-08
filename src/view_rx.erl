-module(view_rx).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {accumulator, subscriptions}).

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

start_link(Subscriptions) ->
	S = #state{subscriptions=Subscriptions},
	gen_server:start_link({local, ?SERVER}, ?MODULE, S, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(S) ->
	subscribe(S#state.subscriptions),	
	{ok, S}.

handle_call({status_change, _View, _Status}=Msg, _From, S) ->
	forward(S#state.accumulator, Msg),
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
