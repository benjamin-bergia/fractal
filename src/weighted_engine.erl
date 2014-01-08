-module(weighted_engine).

-include("engine_api.hrl").

start(Threshold, DeadSum, AliveSum, SuspiciousSum) ->
	[{Status, Sum}|[{SecondStatus, SecondSum}|_T]] = inverted_insertion_sort([{dead, DeadSum}, {alive, AliveSum}, {suspicious, SuspiciousSum}]),
	true.

inverted_insertion_sort(List) ->
	        lists:foldl(fun inverted_insertion/2, [], List).
inverted_insertion(Acc, []) ->
	        [Acc];
inverted_insertion(Acc={_, AccSecond}, List=[{_, Second}|_]) when AccSecond >= Second ->
	        [Acc|List];
inverted_insertion(Acc,[H|T]) ->
	        [H|inverted_insertion(Acc, T)].
