-module(view_acc).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
%% 	From: the id of the source
%% 	Status: the new status of the source
%% @end
%%--------------------------------------------------------------------
forward(Name, Tid, From, Status) ->
	Acc = view_sup:get_pid(Tid, ?MODULE, Name),
	gen_server:cast(Acc, {status_change, From, Status}),
	ok.

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
%% 	ViewID: the id of the source
%% 	Status: the status of the source
%% 	S: the current state
%% @end
%%--------------------------------------------------------------------
handle_cast({status_change, ViewID, Status}, S) ->
	{DList, AList, SList} = update_lists(ViewID, Status, S#state.d_list, S#state.a_list, S#state.s_list),
	ok = view_core:forward(S#state.name, S#state.tid, sum2(DList), sum2(AList), sum2(SList)),
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
%% 	ViewID: the id of the lower view
%% 	Status: the status of the lower view
%% 	DList: a list of 2-tuples containing the dead views
%% 	AList: a list of 2-tuples containing the alive views
%% 	SList: a list of 2-tuples containing the suspicious views
%% @end
%%--------------------------------------------------------------------
update_lists(ViewID, Status, DList, AList, SList) ->
	Weight = get_weight(ViewID, DList, AList, SList),
	D = remove(ViewID, DList),
	A = remove(ViewID, AList),
	S = remove(ViewID, SList),
	update(ViewID, Weight, Status, D, A, S).
-ifdef(TEST).
update_lists_test_() ->
	LowerID = 42,
	Lower = {LowerID, 1},
	Dum = {1, 1},
	[?_assertEqual({[Lower], [], []},
		       update_lists(LowerID, dead, [], [], [Lower])),
	 ?_assertEqual({[], [Lower], []},
		       update_lists(LowerID, alive, [Lower], [], [])),
	 ?_assertEqual({[], [], [Lower]},
		       update_lists(LowerID, suspicious, [], [Lower], [])),
	 ?_assertEqual({[Lower, Dum], [Dum], [Dum]},
		       update_lists(LowerID, dead, [Dum], [Dum, Lower], [Dum])),
	 ?_assertEqual({[Dum], [Lower, Dum], [Dum]},
		       update_lists(LowerID, alive, [Dum], [Dum], [Lower, Dum])),
	 ?_assertEqual({[Dum], [Dum], [Lower, Dum]},
		       update_lists(LowerID, suspicious, [Dum, Lower], [Dum], [Dum]))].
-endif.
	
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Remove a lower view from a list
%% With:
%% 	ViewID: the id of the view
%% 	List: a list of 2-tuples
%% @end
%%--------------------------------------------------------------------
remove(ViewID, List) ->
	lists:keydelete(ViewID, 1, List).
-ifdef(TEST).
remove_test_() ->
	[?_assertEqual([], remove(1, [{1, 2}]))].
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Update the lists of lower views according to their new state
%% With:
%% 	ViewID: the id of the lower view
%% 	Weight: the weight of the lower view
%% 	DList: a list of 2-tuples containing the dead views
%% 	AList: a list of 2-tuples containing the alive views
%% 	SList: a list of 2-tuples containing the suspicious views
%% @end
%%--------------------------------------------------------------------
update(ViewID, Weight, dead, DList, AList, SList) ->
	{append_view(ViewID, Weight, DList), AList, SList};
update(ViewID, Weight, alive, DList, AList, SList) ->
	{DList, append_view(ViewID, Weight, AList), SList};
update(ViewID, Weight, suspicious, DList, AList, SList) ->
	{DList, AList, append_view(ViewID, Weight, SList)}.
-ifdef(TEST).
update_test_() ->
	[?_assertEqual({[{42, 0}], [], []}, update(42, 0, dead, [], [], [])),
	 ?_assertEqual({[], [{42, 0}], []}, update(42, 0, alive, [], [], [])),
	 ?_assertEqual({[], [], [{42, 0}]}, update(42, 0, suspicious, [], [], []))].
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Append a view to a list of view
%% With:
%% 	ViewID: the id of the view
%% 	Weight: the weight of the view
%% 	List: a list of 2-tuples
%% @end
%%--------------------------------------------------------------------
append_view(ViewID, Weight, List) ->
	[{ViewID, Weight}|List].
-ifdef(TEST).
append_view_test_() ->
	[?_assertEqual([{1, 2}], append_view(1, 2, [])),
	 ?_assertEqual([{1, 2}, {3, 4}], append_view(1, 2, [{3, 4}]))].
-endif.

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
-ifdef(TEST).
sum2_test_() ->
	[?_assertEqual(40, sum2([{a, 2}, {"test", 4}, {2, 34}])),
	 ?_assertEqual(0, sum2([{a, 0}, {"test", 0}])),
	 ?_assertEqual(0, sum2([]))].
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Return the weight of a given lower view
%% With:
%% 	ViewID: the id of the lower view
%% 	DList: a list of 2-tuples containing view's IDs and weights
%% 	AList: a list of 2-tuples containing view's IDs and weights
%% 	SList: a list of 2-tuples containing view's IDs and weights
%% @end
%%--------------------------------------------------------------------
get_weight(ViewID, DList, AList, SList) ->
	List = lists:append([DList, AList, SList]),
	{ViewID, Weight} = lists:keyfind(ViewID, 1, List),
	Weight.
-ifdef(TEST).
get_weight_test_() ->
	ViewID = 2,
	Expected = 5,
	DList = [{1, 2},{"hey", 2},{ViewID, Expected},{true, false}],
	AList = [],
	SList = [{true, false}],
	?_assertEqual(Expected, get_weight(ViewID, DList, AList, SList)).
-endif.
