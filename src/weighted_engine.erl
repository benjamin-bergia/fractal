-module(weighted_engine).

-export([start/5]).

start(_Status, Threshold, DeadSum, AliveSum, SuspiciousSum) ->
	[{FirstStatus, FirstSum}|[{_SecondStatus, SecondSum}|_T]] = inverted_insertion_sort([{dead, DeadSum}, {alive, AliveSum}, {suspicious, SuspiciousSum}]),
	case FirstSum == SecondSum of
		true ->
			suspicious;
		false ->
			FirstStatus
	end.

inverted_insertion_sort(List) ->
	lists:foldl(fun inverted_insertion/2, [], List).
inverted_insertion(Acc, []) ->
	[Acc];
inverted_insertion(Acc={_, AccSecond}, List=[{_, Second}|_]) when AccSecond >= Second ->
	[Acc|List];
inverted_insertion(Acc,[H|T]) ->
	[H|inverted_insertion(Acc, T)].

-ifdef(TEST).
-include("test/weighted_engine_tests.hrl").
-endif.
