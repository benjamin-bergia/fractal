-module(view_tx).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {name}).

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

start_link(Name) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, Name, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Name) ->
	S = #state{name=Name},
	{ok, S}.

handle_call({status_change, Status}, _From, S) ->
	propagate(Status, S#state.name),
	{reply, ok, S}.

terminate(_Reason, _State) ->
	ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

propagate(Status, Name) ->
	Pids = resolve(Name),
	Msg = {status_change, Name, Status},
	Txer = fun(Pid) ->
			txer:start(Pid, Msg)
		end,
	list:all(Txer, Pids).

resolve(Name) ->
	Result = gproc:lookup_pids({p, l, Name}),
	{Pids, _Values} = lists:unzip(Result),
	Pids.
