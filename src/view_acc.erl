-module(view_acc).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% State record
%% ------------------------------------------------------------------

-record(state, {name, tid, d_list=[], a_list=[], s_list=[]}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3, forward/4]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_cast/2, terminate/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Start and link to a new accumulator
%% With:
%% 	Name: the accumulator's name
%% 	Tid: the supervisor ETS table Id
%% 	Lowers: a list of 2-tuple containing the Name and Weight
%% 			of each lower view
%% @end
%%--------------------------------------------------------------------
start_link(Name, Tid, Lowers) ->
	gen_server:start_link(?MODULE, {Name, Tid, Lowers}, []).

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Forward a message to this accumulator
%% With:
%% 	Name: the of the receiver
%% 	Tid: the supervisor ETS table Id
%% 	From: the name of the source
%% 	Status: the new status of the source
%% @end
%%--------------------------------------------------------------------
forward(Name, Tid, From, Status) ->
	Acc = view_sup:get_pid(Tid, ?MODULE, Name),
	gen_server:cast(Acc, {status_change, From, Status}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Register the process pid in the supervisor ETS table
%% 	Initialize the state
%% With:
%% 	Name: the accumulator's name
%% 	Tid: the supervisor ETS table Id
%% 	Lowers: a list of 2-tuple containing the Name and Weight
%% 			of each lower view
%% @end
%%--------------------------------------------------------------------
init({Name, Tid, Lowers}) ->
	view_sup:set_pid(Tid, ?MODULE,  Name, self()),
	{ok, #state{name=Name, tid=Tid, d_list=Lowers}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Handle the message forwarded by the receiver 
%% 	Call update_lists to get a a new version of the lower views lists
%% 	Forward this lists to the core
%% 	Update the current state
%% With:
%% 	ViewName: the name of the source
%% 	Status: the status of the source
%% 	S: the current state
%% @end
%%--------------------------------------------------------------------
handle_cast({status_change, ViewName, Status}, S) ->
	{DList, AList, SList} = update_lists(ViewName, Status, S#state.d_list, S#state.a_list, S#state.s_list),
	view_core:forward(S#state.name, S#state.tid, sum2(DList), sum2(AList), sum2(SList)),
	{noreply, S#state{d_list=DList, a_list=AList, s_list=SList}}.

terminate(_Reason, _State) ->
	ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Call get_weight to get the weeight of the lower view
%% 	Call remove for each list to remove the lower view
%% 	Call update to update the lists accordingly to the lower view
%% 		new state
%% With:
%% 	ViewName: the name of the lower view
%% 	Status: the status of the lower view
%% 	DList: a list of 2-tuples containing the dead views
%% 	AList: a list of 2-tuples containing the alive views
%% 	SList: a list of 2-tuples containing the suspicious views
%% @end
%%--------------------------------------------------------------------
update_lists(ViewName, Status, DList, AList, SList) ->
	Weight = get_weight(ViewName, DList, AList, SList),
	D = remove(ViewName, DList),
	A = remove(ViewName, AList),
	S = remove(ViewName, SList),
	update(ViewName, Weight, Status, D, A, S).
	
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Remove a lower view from a list
%% With:
%% 	ViewName: the name of the view
%% 	List: a list of 2-tuples
%% @end
%%--------------------------------------------------------------------
remove(ViewName, List) ->
	lists:keydelete(ViewName, 1, List).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Update the lists of lower views according to their new state
%% With:
%% 	ViewName: the name of the lower view
%% 	Weight: the weight of the lower view
%% 	DList: a list of 2-tuples containing the dead views
%% 	AList: a list of 2-tuples containing the alive views
%% 	SList: a list of 2-tuples containing the suspicious views
%% @end
%%--------------------------------------------------------------------
update(ViewName, Weight, dead, DList, AList, SList) ->
	{append_view(ViewName, Weight, DList), AList, SList};
update(ViewName, Weight, alive, DList, AList, SList) ->
	{DList, append_view(ViewName, Weight, AList), SList};
update(ViewName, Weight, suspicious, DList, AList, SList) ->
	{DList, AList, append_view(ViewName, Weight, SList)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Append a view to a list of view
%% With:
%% 	ViewName: the name of the view
%% 	Weight: the weight of the view
%% 	List: a list of 2-tuples
%% @end
%%--------------------------------------------------------------------
append_view(ViewName, Weight, List) ->
	[{ViewName, Weight}|List].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Sum a list of 2-tuples based on the second element
%% With:
%% 	TupleList: a list of 2-tuples
%% @end
%%--------------------------------------------------------------------
sum2([]) ->
	0;
sum2(TupleList) ->
	{_First, Second} = lists:unzip(TupleList),
	lists:sum(Second).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Return the weight of a given lower view
%% With:
%% 	ViewName: the name of the lower view
%% 	DList: a list of 2-tuples containing view's names and weights
%% 	AList: a list of 2-tuples containing view's names and weights
%% 	SList: a list of 2-tuples containing view's names and weights
%% @end
%%--------------------------------------------------------------------
get_weight(ViewName, DList, AList, SList) ->
	List = lists:append([DList, AList, SList]),
	{ViewName, Weight} = lists:keyfind(ViewName, 1, List),
	Weight.

-ifdef(TEST).
-include("test/view_acc_tests.hrl").
-endif.
