-module(weighted_engine).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, process/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, terminate/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Start and link to a new weighted_engine
%% @end
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Send a synchronous request to the engine
%% With:
%% 	Status: current status of the calling view_core
%% 	Threshold: threshold to use
%% 	StatusList: list of 2-Tuples
%% @end
%%--------------------------------------------------------------------
process(Status, Threshold, StatusList) ->
	gen_server:call(?MODULE, {input, Status, Threshold, StatusList}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
	{ok, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Handle the synchronous calls from view_core
%% With:
%% 	Status: current status of the calling view_core
%% 	Threshold: threshold to use
%% 	StatusList: list of 2-Tuples
%% @end
%%--------------------------------------------------------------------
handle_call({input, Status, Threshold, StatusList}, _From, []) ->
	[First|[Second|_T]] = rev_sort2(StatusList),
	Result = compare(Status, Threshold, First, Second),
	{reply, Result, []}.

terminate(_Reason, _State) ->
	ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%%	Sort a list of 2-tuples based on the second element of each tuple
%% With:
%% 	List: a list of 2-tuples
%% @end
%%--------------------------------------------------------------------
rev_sort2(List) ->
	lists:foldl(fun rev_insert/2, [], List).
rev_insert(Acc, []) ->
	[Acc];
rev_insert(Acc={_, AccSecond}, List=[{_, Second}|_]) when AccSecond >= Second ->
	[Acc|List];
rev_insert(Acc,[H|T]) ->
	[H|rev_insert(Acc, T)].
rev_sort2_test_() ->
	Unsorted = [{e, 5}, {c, 3},
		    {b ,2}, {f, 5},
		    {a, 1}, {d, 4}],
	Sorted = [{f, 5}, {e, 5},
		  {d ,4}, {c, 3},
		  {b, 2}, {a, 1}],
	?_assertEqual(Sorted, rev_sort2(Unsorted)).



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%%	Return suspicious if the 2 elements are equal and greater than
%%		the threshold
%%	Return the first element status if greater than the threshold and the second
%%	Return the current status if the first element is lower than the
%%		threshold
%% With:
%% 	Status: current status name
%% 	Threshold: threshold to use
%% 	StatusA: first status name in the list
%% 	Sum: first status sum of weights
%% 	StatusB: second status name in the list
%% @end
%%--------------------------------------------------------------------
compare(_Status, Threshold, {_StatusA, Sum}, {_StatusB, Sum}) when Sum >= Threshold ->
	suspicious;
compare(_Status, Threshold, {StatusA, Sum}, _) when Sum >= Threshold ->
	StatusA;
compare(Status, _, _, _) ->
	Status.
compare_test_() ->
	[?_assertEqual(dead, compare(dead, 5, {alive, 1}, {dead, 0})),
	 ?_assertEqual(alive, compare(dead, 2, {alive, 5}, {dead, 4})),
	 ?_assertEqual(suspicious, compare(dead, 2, {alive, 4}, {dead, 4}))].
