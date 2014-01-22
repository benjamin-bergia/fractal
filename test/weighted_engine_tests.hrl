-include_lib("eunit/include/eunit.hrl").
-define(STATUSLIST(D, A, S), [{dead, D}, {alive, A}, {suspicious, S}]).

init_test() ->
	?assertEqual(alive, init({dead, 5, ?STATUSLIST(3, 6, 0)})),
	?assertEqual(dead, init({dead, 5, ?STATUSLIST(3, 4, 0)})),
	?assertEqual(suspicious, init({dead, 5, ?STATUSLIST(3, 6, 6)})),
	?assertEqual(dead, init({dead, 5, ?STATUSLIST(3, 3, 3)})).

inverted_insertion_sort_test() ->
	List = [{0, 1}, {a, 5}, {"test", 3}],
	Sorted = [{a, 5}, {"test", 3}, {0, 1}],
	?assertEqual(Sorted, inverted_insertion_sort(List)).

compare_test() ->
	?assertEqual(suspicious, compare(alive, 3, {dead, 5}, {alive, 5})),
	?assertEqual(dead, compare(alive, 3, {dead, 5}, {alive, 4})),
	?assertEqual(alive, compare(alive, 5, {dead, 3}, {alive, 4})),
	?assertEqual(alive, compare(alive, 5, {dead, 3}, {alive, 3})).

