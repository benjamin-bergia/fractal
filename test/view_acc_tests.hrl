-include_lib("eunit/include/eunit.hrl").

update_lists_test() ->
	D = [{a, 3}],
	A = [{ba, 3}, {"test", 15}, {5, "test again"}, {"one", "more"}],
	S = [],
	R = {[{a, 3}], [{ba, 3}, {5, "test again"}, {"one", "more"}], [{"test", 15}]},
	?assertEqual(R, update_lists("test",suspicious, D, A, S)).

get_weight_test() ->
	D = [{a, 3}],
	A = [{ba, 3}, {"test", 15}, {5, "test again"}, {"one", "more"}],
	S = [],
	?assertEqual(15, get_weight("test", D, A, S)).

remove_test() ->
	L = [{ba, 3}, {"test", 15}, {5, "test again"}, {"one", "more"}],
	L2 = [{ba, 3}, {5, "test again"}, {"one", "more"}],
	?assertEqual(L2, remove("test", L)).

