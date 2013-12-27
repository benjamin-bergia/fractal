-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% Unit Tests
%% ------------------------------------------------------------------

routine_test() ->
 	Self = self(),
 	State = #state{view_name=Self, engine=one_for_all, threshold=1, state_name=alive, upper_views=[], lower_views=[{Self, alive, 1}]},
 	NewState = State#state{state_name=dead, lower_views=[{Self, dead, 1}]},
 	Result = {next_state, dead, NewState },
 	Result = routine({Self, dead}, State).

update_state_test() ->
	Self = self(),
	State = #state{view_name=Self, engine=one_for_all, threshold=1, state_name=alive, upper_views=[], lower_views=[{Self, alive, 1}]},
	Result = State#state{state_name=dead, lower_views=[{Self, dead, 1}]},
	Result = update_state({Self, dead}, State).

propagate_test() ->
	Self = self(),
	State = #state{view_name=test, state_name=dead, upper_views=[Self]},
	propagate(State),
	receive
		{_,{test, dead}} ->
			true
	end.

one_for_all_engine_test() ->
	State = #state{state_name=alive},
	Result = State#state{state_name=dead},
        Result = one_for_all_engine(State, {"view", dead}),
        State = one_for_all_engine(State, {"view", alive}).

all_for_one_engine_test() ->
	State = #state{state_name=dead, lower_views=[{one, alive, 1}, {two, dead, 2}, {three, alive, 1}]},
	State2 = State#state{lower_views=[{one, alive, 1}, {two, alive, 3}, {three, alive, 1}]},
	State = all_for_one_engine(State),
	State3 = State2#state{state_name=alive},
	State3 = all_for_one_engine(State2).

weighted_engine_test() ->
	State = #state{state_name=alive, lower_views=[{one, alive, 1}, {two, dead, 2}, {three, alive, 1}]},
        LowerViews2 = [{one, alive, 1}, {two, dead, 3}, {three, alive, 1}],
	State2 = State#state{state_name=suspicious, threshold=2},
	State2 = weighted_engine(State#state{threshold=2}),
	State3 = State#state{state_name=alive, threshold=5},
	State3 = weighted_engine(State#state{threshold=5}),
	State4 = State#state{state_name=dead, lower_views=LowerViews2, threshold=1},
	State4 = weighted_engine(State#state{lower_views=LowerViews2, threshold=1}).

update_lowerviews_state_test() ->
	State = #state{lower_views=[{"first", alive, 1}, {"myview", dead, 2}, {myview, alive, 1}]},
	State2 = #state{lower_views=[{"first", alive, 1}, {"myview", suscpicious, 2}, {myview, alive, 1}]},
	State2 = update_lowerviews_state({"myview", suscpicious}, State).

get_weight_test() ->
        LowerViews = [{"first", alive, 1}, {"myview", dead, 2}, {myview, alive, 1}],
        View = "myview",
        2 = get_weight(View, LowerViews).

inverted_insertion_sort_test() ->
	Unsorted = [{3, 5}, {2, 6}, {1, 7}, {5, 1}],
	SortedSecond = [{1, 7}, {2, 6}, {3, 5}, {5, 1}],
	SortedSecond = inverted_insertion_sort(Unsorted).

sum_weights_test() ->
	Views = [{first, alive, 1}, {second, alive, 3}, {third, suspicious, 2}, {fourth, dead, 2}],
	{alive, 4} = sum_weights(alive, Views),
	{dead, 2} = sum_weights(dead, Views).

list_statenames_test() ->
	Views = [{first, alive, 1}, {second, alive, 3}, {third, suspicious, 2}, {fourth, dead, 2}],
	[alive, dead, suspicious] = list_statenames(Views).
