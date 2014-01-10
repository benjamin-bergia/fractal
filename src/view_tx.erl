-module(view_tx).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {name, tid, view_name}).

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

init({Tid, ViewName}) ->
	S = #state{name=?MODULE, tid=Tid, view_name=ViewName},
	view_sup:set_pid(Tid, ?MODULE, self()),
	{ok, S}.

handle_call({status_change, Status}, _From, S) ->
	propagate(S#state.view_name, Status),
	{reply, ok, S}.

terminate(_Reason, _State) ->
	ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

propagate(ViewName, Status) ->
	Pids = resolve(ViewName),
	Msg = {status_change, ViewName, Status},
	Txer = fun(Pid) ->
			txer:start(Pid, Msg)
		end,
	list:all(Txer, Pids).

resolve(ViewName) ->
	Result = gproc:lookup_pids({p, l, ViewName}),
	{Pids, _Values} = lists:unzip(Result),
	Pids.
