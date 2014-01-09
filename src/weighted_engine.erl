-module(weighted_engine).

-include("engine_api.hrl").

start(Status, Threshold, DeadSum, AliveSum, SuspiciousSum) when DeadSum >= Threshold ; AliveSum >= Threshold ; SuspiciousSum >= Threshold ->
	[{Status, Sum}|[{_SecondStatus, SecondSum}|_T]] = inverted_insertion_sort([{dead, DeadSum}, {alive, AliveSum}, {suspicious, SuspiciousSum}]),
	case Sum = SecondSum of
		true ->
			suspicious;
		false ->
			Status
	end;
start(Status, _Threshold, _DeadSum, _AliveSum, _SuspiciousSum) ->
	Status.

inverted_insertion_sort(List) ->
	lists:foldl(fun inverted_insertion/2, [], List).
inverted_insertion(Acc, []) ->
	[Acc];
inverted_insertion(Acc={_, AccSecond}, List=[{_, Second}|_]) when AccSecond >= Second ->
	[Acc|List];
inverted_insertion(Acc,[H|T]) ->
	[H|inverted_insertion(Acc, T)].
