-module(weighted_engine).
-export([start/3]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Start a new engine
%% With:
%% 	Status: the current status of the view
%% 	Threshold: the threshold to use
%% 	StatusList: List of 2-tuples containing the Sum of Weights for
%% 			each Status
%% @end
%%--------------------------------------------------------------------
start(Status, Threshold, StatusList) ->
	init({Status, Threshold, StatusList}).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%%	Call the sorting function on the StatusList
%%	Call the function to compare the two first results
%% With:
%% 	Status: the current status of the view
%% 	Threshold: the threshold to use
%% 	StatusList: List of 2-tuples containing the Sum of Weights for
%% 			each Status
%% @end
%%--------------------------------------------------------------------
init({Status, Threshold, StatusList}) ->
	[First|[Second|_T]] = inverted_insertion_sort(StatusList),
	compare(Status, Threshold, First, Second).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%%	Sort a list of 2-tuples based on the second element of each tuple
%% With:
%% 	List: a list of 2-tuples
%% @end
%%--------------------------------------------------------------------
inverted_insertion_sort(List) ->
	lists:foldl(fun inverted_insertion/2, [], List).
inverted_insertion(Acc, []) ->
	[Acc];
inverted_insertion(Acc={_, AccSecond}, List=[{_, Second}|_]) when AccSecond >= Second ->
	[Acc|List];
inverted_insertion(Acc,[H|T]) ->
	[H|inverted_insertion(Acc, T)].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%%	Return suspicious if the 2 elements are equal and greater than
%%		the threshold
%%	Return the first element if greater than the threshold and the second
%%	Return the current state if the first element is lower than the
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

-ifdef(TEST).
-include("test/weighted_engine_tests.hrl").
-endif.
