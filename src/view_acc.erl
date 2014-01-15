-module(view_acc).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {name, tid, d_list=[], a_list=[], s_list=[]}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, forward/4]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, terminate/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

forward(Name, Tid, From, Status) ->
	Acc = view_sup:get_pid(Tid, ?MODULE, Name),
	gen_server:call(Acc, {status_change, From, Status}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({Name, Tid, DList, AList, SList}) ->
	S = #state{name=Name, tid=Tid, d_list=DList, a_list=AList, s_list=SList},
	view_sup:set_pid(Tid, ?MODULE,  Name, self()),
	{ok, S}.

handle_call({status_change, ViewName, Status}, _From, S) ->
	{DList, AList, SList} = update_lists(ViewName, Status, S#state.d_list, S#state.a_list, S#state.s_list),
	{DSum, ASum, SSum} = sum(DList, AList, SList),
	view_core:forward(S#state.name, S#state.tid, DSum, ASum, SSum),
	{reply, ok, S#state{d_list=DList, a_list=AList, s_list=SList}}.

terminate(_Reason, _State) ->
	ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

update_lists(ViewName, Status, DList, AList, SList) ->
	Weight = get_weight(ViewName, DList, AList, SList),
	D = remove(ViewName, DList),
	A = remove(ViewName, AList),
	S = remove(ViewName, SList),
	update(ViewName, Weight, Status, D, A, S).
	
remove(ViewName, List) ->
	NewList = lists:keydelete(ViewName, 1, List),
	case NewList /= List of
		true ->
			NewList;
		false ->
			List
	end.

update(ViewName, Weight, dead, DList, AList, SList) ->
	NewDList = append_view(ViewName, Weight, DList),
	{NewDList, AList, SList};
update(ViewName, Weight, alive, DList, AList, SList) ->
	NewAList = append_view(ViewName, Weight, AList),
	{DList, NewAList, SList};
update(ViewName, Weight, suspicious, DList, AList, SList) ->
	NewSList = append_view(ViewName, Weight, SList),
	{DList, AList, NewSList}.

append_view(ViewName, Weight, List) ->
	[{ViewName, Weight}|List].

sum(DList, AList, SList) ->
	DSum = sum2(DList),
	ASum = sum2(AList),
	SSum = sum2(SList),
	{DSum, ASum, SSum}.

sum2([]) ->
	0;
sum2(TupleList) ->
	{_First, Second} = lists:unzip(TupleList),
	lists:sum(Second).

get_weight(ViewName, DList, AList, SList) ->
	List = lists:append([DList, AList, SList]),
	{ViewName, Weight} = lists:keyfind(ViewName, 1, List),
	Weight.

-ifdef(TEST).
-include("test/view_acc_tests.hrl").
-endif.
