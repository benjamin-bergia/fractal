-module(weighted_engine).

-export([start/3]).

start(Status, Threshold, StatusList) ->
	init({Status, Threshold, StatusList}).

init({Status, Threshold, StatusList}) ->
	[First|[Second|_T]] = inverted_insertion_sort(StatusList),
	compare(Status, Threshold, First, Second).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

inverted_insertion_sort(List) ->
	lists:foldl(fun inverted_insertion/2, [], List).
inverted_insertion(Acc, []) ->
	[Acc];
inverted_insertion(Acc={_, AccSecond}, List=[{_, Second}|_]) when AccSecond >= Second ->
	[Acc|List];
inverted_insertion(Acc,[H|T]) ->
	[H|inverted_insertion(Acc, T)].

compare(_Status, Threshold, {_StatusA, Sum}, {_StatusB, Sum}) when Sum >= Threshold ->
	suspicious;
compare(_Status, Threshold, {StatusA, Sum}, _) when Sum >= Threshold ->
	StatusA;
compare(Status, _, _, _) ->
	Status.

-ifdef(TEST).
-include("test/weighted_engine_tests.hrl").
-endif.
