-module(view_acc).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {name, view, deads=[], alives=[], suspicious=[]}).

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

start_link(Name, ViewName) ->
	S = #state{name=Name, view=View},
	gen_server:start_link({local, ?SERVER}, ?MODULE, S, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(S) ->
	{ok, S}.

handle_call({status_change, ViewName, Status}, _From, S) ->
	{Deads, Alives, Suspicious} = update_lists(ViewName, Status, S#state.deads, S#state.alives, S#state.suspicious),
	{DeadSum, AliveSum, SuspiciousSum} = sum(Deads, Alives, Suspicious),
	forward(S#state.name, View, DeadSum, AliveSum, SuspiciousSum),
	{reply, ok, S#state{deads=Deads, alives=Alives, suscpicious=Suspicious}}.

terminate(_Reason, _State) ->
	ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

update_lists(ViewName, Status, Deads, Alives, Suspicious) ->
	D = remove(ViewName, Deads),
	A = remove(ViewName, Alives),
	S = remove(ViewName, Suspicious),
	update(ViewName, Status, D, A, S).
	
remove(ViewName, List) ->
	NewList = lists:keydelete(ViewName, 1, List),
	case NewList /= List of
		true ->
			{true, NewList};
		false ->
			{false, NewList}
	end.

update(ViewName, dead, Deads, Alives, Suspicious) ->
	NewDeads = append(ViewName, dead, Deads),
	{NewDeads, Alives, Suspicious};
update(ViewName, alive, Deads, Alives, Suspicious) ->
	NewAlives = append(ViewName, alive, Alives),
	{Deads, NewAlives, Suspicious};
update(ViewName, suspicious, Deads, Alives, Suspicious) ->
	NewSuspicious = append(ViewName, suspicious, Suspicious),
	{Deads, Alives, NewSuspicious}.

append(ViewName, Status, List) ->
	[{ViewName, Status}|List].

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


