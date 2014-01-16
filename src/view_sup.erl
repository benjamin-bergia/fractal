-module(view_sup).

-behaviour(supervisor).

%% API
-export([start_link/1,
	 set_pid/3, set_pid/4,
	 get_pid/2, get_pid/3]).

%% Supervisor callbacks
-export([init/2]).

-define(TX(Tid, ViewName), {view_tx, {view_tx, start_link, [{Tid, ViewName}]}, permanent, 5000, worker, [view_tx]}).
-define(CORE(Tid, DE, DT, AE, AT, SE, ST), {view_core, {view_core, start_link, [{Tid, {DE, DT}, {AE, AT}, {SE, ST}}]}, permanent, 5000, worker, [view_core]}).
-define(ACC(Name, Tid, Lowers), {Name, {view_acc, start_link, [{Name, Tid, Lowers, Lowers, Lowers}]}, permanent, 5000, worker, [view_acc]}).
-define(RX(Name, Tid, Acc, Subs), {Name, {view_rx, start_link, [{Name, Tid, Acc, Subs}]}, permanent, 5000, worker, [view_rx]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(ViewName, Lowers) ->
	Tid = create_table(),
	{Subs, _Weights} = lists:unzip(Lowers),
	View = [?TX(Tid, ViewName),
		?CORE(Tid, weighted_engine, 1, weighted_engine, 1, weighted_engine, 1),
		?ACC(dead_acc, Tid, Lowers),
		?RX(dead_rx, Tid, dead_acc, Subs),
		?ACC(alive_acc, Tid, Lowers),
		?RX(alive_rx, Tid, alive_acc, Subs),
		?ACC(suspicious_acc, Tid, Lowers),
		?RX(suspicious_rx, Tid, suspicious_acc, Subs)],
	{ok, {{one_for_one, 5, 10}, View}}.

create_table() ->
	ets:new(childs, [set, public, {keypos, 1}]).

set_pid(Tid, Module, Pid) ->
	set_pid(Tid, Module, Module, Pid).
set_pid(Tid, Module, Name, Pid) ->
	ets:insert(Tid, {{Module, Name}, Pid}).

get_pid(Tid, Module) ->
	get_pid(Tid, Module, Module).
get_pid(Tid, Module, Name) ->
	[{{Module, Name}, Pid}] = ets:lookup(Tid, {Module, Name}),
	Pid.
