-module(view_acc).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {name, tid, deads=[], alives=[], suspicious=[]}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, notify_status/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, terminate/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

notify_status(To, From, Status) ->
	gen_server:call(To, {status_change, From, Status}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({Name, Tid, Deads, Alives, Suspicious}) ->
	S = #state{name=Name, tid=Tid, deads=Deads, alives=Alives, suspicious=Suspicious},
	view_sup:set_pid(Tid, Name, self()),
	{ok, S}.

handle_call({status_change, ViewName, Status}, _From, S) ->
	{Deads, Alives, Suspicious} = update_lists(ViewName, Status, S#state.deads, S#state.alives, S#state.suspicious),
	{DeadSum, AliveSum, SuspiciousSum} = sum(Deads, Alives, Suspicious),
	Core = view_sup:get_pid(S#state.tid, view_core),
	forward(Core, S#state.name, DeadSum, AliveSum, SuspiciousSum),
	{reply, ok, S#state{deads=Deads, alives=Alives, suspicious=Suspicious}}.

terminate(_Reason, _State) ->
	ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

update_lists(ViewName, Status, Deads, Alives, Suspicious) ->
	Weight = get_weight(ViewName, Deads, Alives, Suspicious),
	D = remove(ViewName, Deads),
	A = remove(ViewName, Alives),
	S = remove(ViewName, Suspicious),
	update(ViewName, Weight, Status, D, A, S).
	
remove(ViewName, List) ->
	NewList = lists:keydelete(ViewName, 1, List),
	case NewList /= List of
		true ->
			NewList;
		false ->
			List
	end.

update(ViewName, Weight, dead, Deads, Alives, Suspicious) ->
	NewDeads = append_view(ViewName, Weight, Deads),
	{NewDeads, Alives, Suspicious};
update(ViewName, Weight, alive, Deads, Alives, Suspicious) ->
	NewAlives = append_view(ViewName, Weight, Alives),
	{Deads, NewAlives, Suspicious};
update(ViewName, Weight, suspicious, Deads, Alives, Suspicious) ->
	NewSuspicious = append_view(ViewName, Weight, Suspicious),
	{Deads, Alives, NewSuspicious}.

append_view(ViewName, Weight, List) ->
	[{ViewName, Weight}|List].

sum(Deads, Alives, Suspicious) ->
	DeadSum = sum2(Deads),
	AliveSum = sum2(Alives),
	SuspiciousSum = sum2(Suspicious),
	{DeadSum, AliveSum, SuspiciousSum}.

sum2(TupleList) ->
	{_First, Second} = lists:unzip(TupleList),
	lists:sum(Second).

forward(Core, Name, DeadSum, AliveSum, SuspiciousSum) ->
	view_core:forward(Core, Name, DeadSum, AliveSum, SUspiciousSum).

get_weight(ViewName, Deads, Alives, Suspicious) ->
	List = lists:append([Deads, Alives, Suspicious]),
	{ViewName, Weight} = lists:keyfind(ViewName, 1, List),
	Weight.

-ifdef(TEST).
-include("test/view_acc_tests.hrl").
-endif.
