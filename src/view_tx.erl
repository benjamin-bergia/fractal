-module(view_tx).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {name, tid, view_name}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2, forward/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, terminate/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Tid, ViewName) ->
	gen_server:start_link(?MODULE, {Tid, ViewName}, []).

forward(Tid, Status) ->
	TX = view_sup:get_pid(Tid, ?MODULE),
	gen_server:call(TX, {status_change, Status}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({Tid, ViewName}) ->
	view_sup:set_pid(Tid, ?MODULE, self()),
	{ok, #state{name=?MODULE, tid=Tid, view_name=ViewName}}.

handle_call({status_change, Status}, _From, S) ->
	view_rx:forward(S#state.view_name, Status),
	{reply, ok, S}.

terminate(_Reason, _State) ->
	ok.
