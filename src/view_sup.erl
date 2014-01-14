-module(view_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, set_pid/3, get_pid/2]).

%% Supervisor callbacks
-export([init/1]).

-define(TX(Tid, ViewName), {view_tx, {view_tx, start_link, [{Tid, ViewName}]}, permanent, 5000, worker, [view_tx]}).
-define(CORE(Tid, DE, DT, AE, AT, SE, ST), {view_core, {view_core, start_link, [{Tid, {DE, DT}, {AE, AT}, {SE, ST}}]}, permanent, 5000, worker, [view_core]}).
-define(ACC(Name, Tid, D, A, S), {Name, {view_acc, start_link, [{Name, Tid, D, A, S}]}, permanent, 5000, worker, [view_acc]}).
-define(RX(Name, Tid, Acc, Subs), {Name, {view_rx, start_link, [{Name, Tid, Acc, Subs}]}, permanent, 5000, worker, [view_rx]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(ViewName) ->
    supervisor:start_link(?MODULE, [ViewName]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(ViewName) ->
	Tid = create_table(),
	View = [?TX(Tid, ViewName),
		?CORE(Tid, weighted_engine, 1, weighted_engine, 1, weighted_engine, 1),
		?ACC(dead_acc, Tid, [{"test", 1}], [{"test", 1}], [{"test", 1}]),
		?RX(dead_rx, Tid, dead_acc, ["test"]),
		?ACC(alive_acc, Tid, [{"test", 1}], [{"test", 1}], [{"test", 1}]),
		?RX(alive_rx, Tid, alive_acc, ["test"]),
		?ACC(suspicious_acc, Tid, [{"test", 1}], [{"test", 1}], [{"test", 1}]),
		?RX(suspicious_rx, Tid, suspicious_acc, ["test"])],
	{ok, {{one_for_one, 5, 10}, View}}.

create_table() ->
	ets:new(childs, [set, public, {keypos, 1}]).

set_pid(Tid, Name, Pid) ->
	ets:insert(Tid, {Name, Pid}).

get_pid(Tid, Name) ->
	[{Name, Pid}] = ets:lookup(Tid, Name),
	Pid.
