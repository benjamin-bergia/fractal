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

update_test() ->
	ViewName = "test",
	Weight = 3,
	DList = [],
	AList = [{true, 0}],
	SList = [{false, 1}, {"hello", "world"}],
	?assertEqual({[{ViewName, Weight}], AList, SList}, update(ViewName, Weight, dead, DList, AList, SList)),
	?assertEqual({DList, [{ViewName, Weight}|AList], SList}, update(ViewName, Weight, alive, DList, AList, SList)),
	?assertEqual({DList, AList, [{ViewName, Weight}|SList]}, update(ViewName, Weight, suspicious, DList, AList, SList)).

append_view_test() ->
	List = [{"test", 3}],
	?assertEqual(List, append_view("test", 3, [])).

sum2_test() ->
	TupleList = [{a, 2}, {"test", 4}, {2, 34}],
	?assertEqual(40, sum2(TupleList)),
	?assertEqual(0, sum2([])).
