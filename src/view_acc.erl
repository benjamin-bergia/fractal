-module(view_acc).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {name, core, deads=[], alives=[], suspicious=[]}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, terminate/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Name, ViewCore) ->
	S = #state{name=Name, core=ViewCore},
	gen_server:start_link({local, ?SERVER}, ?MODULE, S, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(S) ->
	{ok, S}.

handle_call({status_change, ViewName, Status}, _From, S) ->
	{Deads, Alives, Suspicious} = update_lists(ViewName, Status, S#state.deads, S#state.alives, S#state.suspicious),
	{DeadSum, AliveSum, SuspiciousSum} = sum(Deads, Alives, Suspicious),
	forward(S#state.name, ViewName, DeadSum, AliveSum, SuspiciousSum),
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
	{true, NewList} = lists:keydelete(ViewName, 1, List),
	case NewList /= List of
		true ->
			{true, NewList};
		false ->
			{false, NewList}
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

forward(Name, View, DeadSum, AliveSum, SuspiciousSum) ->
	Msg = {Name, {dead, DeadSum}, {alive, AliveSum}, {suspicious, SuspiciousSum}},
	gen_fsm:send(View, Msg).

get_weight(ViewName, Deads, Alives, Suspicious) ->
	List = lists:append([Deads, Alives, Suspicious]),
	{ViewName, Weight} = lists:keyfind(ViewName, 1, List),
	Weight.

-ifdef(TEST).
-include("test/view_acc_tests.hrl").
-endif.
