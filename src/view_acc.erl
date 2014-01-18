-module(view_acc).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {name, tid, d_list=[], a_list=[], s_list=[]}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/5, forward/4]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_cast/2, terminate/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Name, Tid, DList, AList, SList) ->
	gen_server:start_link(?MODULE, {Name, Tid, DList, AList, SList}, []).

forward(Name, Tid, From, Status) ->
	Acc = view_sup:get_pid(Tid, ?MODULE, Name),
	gen_server:cast(Acc, {status_change, From, Status}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({Name, Tid, DList, AList, SList}) ->
	view_sup:set_pid(Tid, ?MODULE,  Name, self()),
	{ok, #state{name=Name, tid=Tid, d_list=DList, a_list=AList, s_list=SList}}.

handle_cast({status_change, ViewName, Status}, S) ->
	{DList, AList, SList} = update_lists(ViewName, Status, S#state.d_list, S#state.a_list, S#state.s_list),
	view_core:forward(S#state.name, S#state.tid, sum2(DList), sum2(AList), sum2(SList)),
	{noreply, S#state{d_list=DList, a_list=AList, s_list=SList}}.

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
	lists:keydelete(ViewName, 1, List).

update(ViewName, Weight, dead, DList, AList, SList) ->
	{append_view(ViewName, Weight, DList), AList, SList};
update(ViewName, Weight, alive, DList, AList, SList) ->
	{DList, append_view(ViewName, Weight, AList), SList};
update(ViewName, Weight, suspicious, DList, AList, SList) ->
	{DList, AList, append_view(ViewName, Weight, SList)}.

append_view(ViewName, Weight, List) ->
	[{ViewName, Weight}|List].

%% Sum second elements of a 2-Tuple List
sum2([]) ->
	0;
sum2(TupleList) ->
	{_First, Second} = lists:unzip(TupleList),
	lists:sum(Second).

%% Return the weight associated with a given ViewName
get_weight(ViewName, DList, AList, SList) ->
	List = lists:append([DList, AList, SList]),
	{ViewName, Weight} = lists:keyfind(ViewName, 1, List),
	Weight.

-ifdef(TEST).
-include("test/view_acc_tests.hrl").
-endif.
