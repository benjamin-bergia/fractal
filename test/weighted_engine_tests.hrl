-include_lib("eunit/include/eunit.hrl").

start_test() ->
	?assertEqual(alive, start(dead, 5, 3, 6, 0)),
	?assertEqual(dead, start(dead, 5, 3, 4, 0)),
	?assertEqual(suspicious, start(dead, 5, 3, 6, 6)),
	?assertEqual(dead, start(dead, 5, 3, 3, 3)).

inverted_insertion_sort_test() ->
	List = [{0, 1}, {a, 5}, {"test", 3}],
	Sorted = [{a, 5}, {"test", 3}, {0, 1}],
	?assertEqual(Sorted, inverted_insertion_sort(List)).
