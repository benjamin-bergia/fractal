-module(view_sup).
-behaviour(supervisor).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/10,
	 set_pid/3, set_pid/4,
	 get_pid/2, get_pid/3]).

%% ------------------------------------------------------------------
%% Supervisor Function Exports
%% ------------------------------------------------------------------
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Start and link to a new view supervisor
%% With:
%% 	ViewID: the id of the new view
%% 	DE: the engine to use when dead
%% 	DT: the threshold to use when dead
%% 	DL: a list of 2-Tuples containing the lower views and
%% 			weights associated to them
%% 	AE: the engine to use when alive
%% 	AT: the threshold to use when alive
%% 	AL: a list of 2-Tuples containing the lower views and
%% 			weights associated to them
%% 	SE: the engine to use when suspicious
%% 	ST: the threshold to use when suspicious
%% 	SL: a list of 2-Tuples containing the lower views and
%% 			weights associated to them
%% @end
%%--------------------------------------------------------------------
start_link(ViewID, DE, DT, DL, AE, AT, AL, SE, ST, SL) ->
    supervisor:start_link(?MODULE, {ViewID, DE, DT, DL, AE, AT, AL, SE, ST, SL}).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Notify the View from a Status change (called by a lower view)
%% With:
%% 	From: id of calling view
%% 	Status: new status of the calling view
%% @end
%%--------------------------------------------------------------------

init({ViewID, E, T, L}) ->
	Tid = create_table(),
	{S, _Weights} = lists:unzip(L), % Generate the susbscription list
	View = [{view_tx,	{view_tx,	start_link, [Tid, ViewID]},	permanent, 5000, worker, [view_tx]},
		{view_core, 	{view_core,	start_link, [Tid, E, T]},	permanent, 5000, worker, [view_core]},
		{all_acc,	{view_acc,	start_link, [all, Tid, L]},	permanent, 5000, worker, [view_acc]},
		{all_rx, 	{view_rx,	start_link, [all, Tid, S]},	permanent, 5000, worker, [view_rx]}],
	{ok, {{one_for_one, 5, 10}, View}};
init({ViewID, DE, DT, DL, AE, AT, AL, SE, ST, SL}) ->
	Tid = create_table(),
	{DS, _Weights} = lists:unzip(DL), % Generate the susbscription list for the dead state
	{AS, _Weights} = lists:unzip(AL), % Generate the susbscription list for the alive state
	{SS, _Weights} = lists:unzip(SL), % Generate the susbscription list for the suspicious state
	View = [{view_tx,	{view_tx,	start_link, [Tid, ViewID]},			permanent, 5000, worker, [view_tx]},
		{view_core, 	{view_core,	start_link, [Tid, DE, DT, AE, AT, SE, ST]},	permanent, 5000, worker, [view_core]},
		{dead_acc,	{view_acc,	start_link, [dead, Tid, DL]},			permanent, 5000, worker, [view_acc]},
		{dead_rx,	{view_rx,	start_link, [dead, Tid, DS]},			permanent, 5000, worker, [view_rx]},
		{alive_acc,	{view_acc,	start_link, [alive, Tid, AL]},			permanent, 5000, worker, [view_acc]},
		{alive_rx,	{view_rx,	start_link, [alive, Tid, AS]},			permanent, 5000, worker, [view_rx]},
		{suspicious_acc,{view_acc,	start_link, [suspicious, Tid, SL]},		permanent, 5000, worker, [view_acc]},
		{suspicious_rx, {view_rx,	start_link, [suspicious, Tid, SS]},		permanent, 5000, worker, [view_rx]}],
	{ok, {{one_for_one, 5, 10}, View}}.

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Create a new ETS table to store the child's pids
%% @end
%%--------------------------------------------------------------------
create_table() ->
	ets:new(childs, [set, public, {keypos, 1}]).

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Register a new pid in the ETS table
%% With:
%% 	Tid: ETS table Id
%% 	Module: name of the calling module
%% 	Name: name of the calling process
%% 	Pid: process Pid
%% @end
%%--------------------------------------------------------------------
set_pid(Tid, Module, Pid) ->
	set_pid(Tid, Module, Module, Pid).
set_pid(Tid, Module, Name, Pid) ->
	ets:insert(Tid, {{Module, Name}, Pid}).

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Return the pid stored in the ETS table
%% With:
%% 	Tid: ETS table Id
%% 	Module: name of the calling module
%% 	Name: name of the calling process
%% @end
%%--------------------------------------------------------------------
get_pid(Tid, Module) ->
	get_pid(Tid, Module, Module).
get_pid(Tid, Module, Name) ->
	[{{Module, Name}, Pid}] = ets:lookup(Tid, {Module, Name}),
	Pid.
