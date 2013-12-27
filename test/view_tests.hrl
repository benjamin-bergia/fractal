-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% Unit Tests
%% ------------------------------------------------------------------

routine_test() ->
	Self = self(),
	Result = routine({Self, dead}, {Self, {one_for_all, 1}, alive, [], [{Self, alive, 1}]}),
	{next_state, dead, {Self, {one_for_all, 1}, dead, [], [{Self, dead, 1}]}} = Result.

update_state_test() ->
	Self = self(),
	{Self, {one_for_all, 1}, dead, [], [{Self, dead, 1}]} = update_state({Self, dead}, {Self, {one_for_all, 1}, alive, [], [{Self, alive, 1}]}).

propagate_test() ->
	Self = self(),
	State = #state{view_name=test, state_name=dead, upper_views=Self},
	propagate(State),
	receive
		{_,{Self, dead}} ->
			true
	end.

one_for_all_engine_test() ->
        dead = one_for_all_engine(alive, {"view", dead}),
        alive = one_for_all_engine(alive, {"view", alive}).

all_for_one_engine_test() ->
	State = #state{state_name=dead},
        LowerViews = [{one, alive, 1}, {two, dead, 2}, {three, alive, 1}],
        LowerViews2 = [{one, alive, 1}, {two, alive, 3}, {three, alive, 1}],
	#state{state_name=dead, lower_views=LowerViews} = all_for_one_engine(State#state{lower_views=LowerViews}),
	#state{state_name=alive, lower_views=LowerViews2} = all_for_one_engine(State#state{lower_views=LowerViews2}).

weighted_engine_test() ->
	State = #state{state_name=alive},
        LowerViews = [{one, alive, 1}, {two, dead, 2}, {three, alive, 1}],
        LowerViews2 = [{one, alive, 1}, {two, dead, 3}, {three, alive, 1}],
	#state{state_name=suspicious, lower_views=LowerViews, threshold=2} = weighted_engine(State#state{lower_views=LowerViews, threshold=2}),
	#state{state_name=alive, lower_views=LowerViews, threshold=5} = weighted_engine(State#state{lower_views=LowerViews, threshold=5}),
	#state{state_name=dead, lower_views=LowerViews2, threshold=1} = weighted_engine(State#state{lower_views=LowerViews2, threshold=1}).

update_lowerviews_state_test() ->
	State#state{lower_views=[{"first", alive, 1}, {"myview", dead, 2}, {myview, alive, 1}]}
	#state{lower_views=[{"first", alive, 1}, {"myview", suscpicious, 2}, {myview, alive, 1}]} = update_lowerviews_state({"myview", suscpicious}, State).

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
