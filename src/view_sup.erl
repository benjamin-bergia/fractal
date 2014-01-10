-module(view_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, set_pid/3, get_pid/2]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Tid = create_table(),
	ViewTX = 	{view_tx, 	{view_tx,	start_link, []},	permanent, 5000, worker, [view_tx]},
	ViewCore = 	{view_core, 	{view_core,	start_link, []},	permanent, 5000, worker, [view_core]},
	DeadAcc = 	{dead_acc, 	{view_acc,	start_link, []},	permanent, 5000, worker, [view_acc]},
	DeadRX = 	{dead_rx, 	{view_rx,	start_link, []},	permanent, 5000, worker, [view_rx]},
	AliveAcc = 	{alive_acc, 	{view_acc, 	start_link, []},	permanent, 5000, worker, [view_acc]},
	AliveRX = 	{alive_rx, 	{view_rx,	start_link, []},	permanent, 5000, worker, [view_rx]},
	SuspiciousAcc = {suspicious_acc,{view_acc,	start_link, []},	permanent, 5000, worker, [view_acc]},
	SuspiciousRX = 	{suspicious_rx, {view_rx,	start_link, []}, 	permanent, 5000, worker, [view_rx]},
	View = [ViewTX,		ViewCore,
	       	DeadAcc,	DeadRX,
	       	AliveAcc,	AliveRX,
	       	SuspiciousAcc,	SuspiciousRX],
	{ok, {{one_for_one, 5, 10}, View}}.

create_table() ->
	ets:new(childs, [set, public, {keypos, 1}]).

set_pid(Tid, Name, Pid) ->
	ets:insert(Tid, {Name, Pid}).

get_pid(Tid, Name) ->
	[{Name, Pid}] = ets:lookup(Tid, Name),
	Pid.
