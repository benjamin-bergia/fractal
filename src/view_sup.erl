-module(view_sup).

-behaviour(supervisor).

%% API
-export([start_link/1,
	 set_pid/3, set_pid/4,
	 get_pid/2, get_pid/3]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init({ViewName, Lowers, DE, DT, AE, AT, SE, ST}) ->
	Tid = create_table(ViewName),
	{Subs, _Weights} = lists:unzip(Lowers),
	View = [{view_tx,	{view_tx,	start_link, [Tid, ViewName]},				permanent, 5000, worker, [view_tx]},
		{view_core, 	{view_core,	start_link, [Tid, DE, DT, AE, AT, SE, ST]},		permanent, 5000, worker, [view_core]},
		{dead_acc,	{view_acc,	start_link, [{dead, Tid, Lowers, [], []}]},		permanent, 5000, worker, [view_acc]},
		{dead_rx,	{view_rx,	start_link, [dead, Tid, Subs]},				permanent, 5000, worker, [view_rx]},
		{alive_acc,	{view_acc,	start_link, [{alive, Tid, Lowers, [], []}]},		permanent, 5000, worker, [view_acc]},
		{alive_rx,	{view_rx,	start_link, [alive, Tid, Subs]},			permanent, 5000, worker, [view_rx]},
		{suspicious_acc,{view_acc,	start_link, [{suspicious, Tid, Lowers, [], []}]},	permanent, 5000, worker, [view_acc]},
		{suspicious_rx, {view_rx,	start_link, [suspicious, Tid, Subs]},			permanent, 5000, worker, [view_rx]}],
	{ok, {{one_for_one, 5, 10}, View}}.

create_table(ViewName) ->
	ets:new(ViewName, [set, public, {keypos, 1}]).

set_pid(Tid, Module, Pid) ->
	set_pid(Tid, Module, Module, Pid).
set_pid(Tid, Module, Name, Pid) ->
	ets:insert(Tid, {{Module, Name}, Pid}).

get_pid(Tid, Module) ->
	get_pid(Tid, Module, Module).
get_pid(Tid, Module, Name) ->
	[{{Module, Name}, Pid}] = ets:lookup(Tid, {Module, Name}),
	Pid.
