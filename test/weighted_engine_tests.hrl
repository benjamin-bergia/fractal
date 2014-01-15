-include_lib("eunit/include/eunit.hrl").
-define(STATUSLIST(D, A, S), [{dead, D}, {alive, A}, {suspicious, S}]).

init_test() ->
	?assertEqual({stop, alive}, init(dead, 5, ?STATUSLIST(3, 6, 0))),
	?assertEqual({stop, dead}, init(dead, 5, ?STATUSLIST(3, 4, 0))),
	?assertEqual({stop, suspicious}, init(dead, 5, ?STATUSLIST(3, 6, 6))),
	?assertEqual({stop, dead}, init(dead, 5, ?STATUSLIST(3, 3, 3))).

inverted_insertion_sort_test() ->
	List = [{0, 1}, {a, 5}, {"test", 3}],
	Sorted = [{a, 5}, {"test", 3}, {0, 1}],
	?assertEqual(Sorted, inverted_insertion_sort(List)).
