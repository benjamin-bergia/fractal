-module(view_fsm).
-behaviour(gen_fsm).

-export([start_link/2, init/1, alive/2, dead/2, suspicious/2]).
%%Testing export
-export([]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start_link(Name, StateData) ->
	gen_fsm:start_link(Name, ?MODULE, StateData, []).

init(StateData) ->
	{ok, dead, StateData}.

%% View States 
alive(Message, StateData) ->
	routine(Message, StateData).
dead(Message, StateData) ->
	routine(Message, StateData).
suspicious(Message, StateData) ->
	routine(Message, StateData).
%% 

%% In all the states the same function is used 
routine(Message, StateData) ->
	{_, _, NextState, _, _} = NewStateData = update_status(Message, StateData),
	propagate(NewStateData),
	{next_state, NextState, NewStateData}.
-ifdef(TEST).
routine_test() ->
	Self = self(),
	Result = routine({Self, dead}, {Self, {one_for_all, 1}, alive, [], [{Self, alive, 1}]}),
	{next_state, dead, {Self, {one_for_all, 1}, dead, [], [{Self, dead, 1}]}} = Result.
-endif.
%%
	
%% Call the different engines and returns the new State Data
update_status(Message, {Name, {Engine, Threshold}, State, UpperViews, LowerViews}) ->
	NewLowerViews = update_lowerviews_state(Message, LowerViews),
	case Engine of 
		one_for_all ->
			NewState = one_for_all_engine(State, Message);
		all_for_one ->
			NewState = all_for_one_engine(State, NewLowerViews);
		weighted ->
			NewState = weighted_engine(Threshold, State, NewLowerViews)
	end,
	{Name, {Engine, Threshold}, NewState, UpperViews, NewLowerViews}.
-ifdef(TEST).
update_status_test() ->
	Self = self(),
	{Self, {one_for_all, 1}, dead, [], [{Self, dead, 1}]} = update_status({Self, dead}, {Self, {one_for_all, 1}, alive, [], [{Self, alive, 1}]}).
-endif.
%%

%% Send a state update to all the upper views 
propagate({Name, _, State, UpperViews, _}) ->
	Send = fun(Target) ->
			gen_fsm:send_event(Target, {Name, State}),
			true
		end,
	lists:all(Send, UpperViews).
-ifdef(TEST).
propagate_test() ->
	Self = self(),
	propagate({Self, test, dead, [Self], []}),
	receive
		{_,{Self, dead}} ->
			true
	end.
-endif.
%%

%% one_for_all: State changes when at least one of the lower Views has a different State
one_for_all_engine(State, {_, ViewState}) ->
	case ViewState /= State of 
		true ->
			ViewState;
		false ->
			State
	end.
-ifdef(TEST).
one_for_all_engine_test() ->
	dead = one_for_all_engine(alive, {"view", dead}),
	alive = one_for_all_engine(alive, {"view", alive}).
-endif.
%%

%% all_for_one: State changes when all the lower Views have the same State
all_for_one_engine(State, LowerViews) -> 
	{_, [NewState|States], _} = lists:unzip3(LowerViews),
	F = fun(X) when X == NewState ->
			true;
		(_) ->
			false
		end,
	case lists:all(F, States) of
		true ->
			NewState;
		false ->
			State
	end.
-ifdef(TEST).
all_for_one_engine_test() ->
	LowerViews = [{one, alive, 1}, {two, dead, 2}, {three, alive, 1}],
	LowerViews2 = [{one, alive, 1}, {two, alive, 3}, {three, alive, 1}],
	dead = all_for_one_engine(dead, LowerViews),
	alive = all_for_one_engine(dead, LowerViews2).
-endif.
%%

%% weighted State change when the weight sum of the lower views sharing a same state is equal or greater than the threshold value
weighted_engine(Threshold, State, LowerViews) ->
	States = list_states(LowerViews),
	F = fun(S) ->
			sum_weights(S, LowerViews)
		end,
	StateSums = lists:map(F, States),
	[{NewState, Sum}|[{_, Sum2}]] = inverted_insertion_sort(StateSums), 
	if
		%% If less than the Threshold, no state change
		Sum < Threshold ->
			State;
		%% In case of equal weight sum return suspicious.
		Sum == Sum2 ->
			suspicious;
		Sum >= Threshold -> 
			NewState
	end.
-ifdef(TEST).
weighted_engine_test() ->
	LowerViews = [{one, alive, 1}, {two, dead, 2}, {three, alive, 1}],
	LowerViews2 = [{one, alive, 1}, {two, dead, 3}, {three, alive, 1}],
	suspicious = weighted_engine(2, alive, LowerViews),
	alive = weighted_engine(5, alive, LowerViews),
	dead = weighted_engine(1, alive, LowerViews2).
-endif.
%%

%% Update the state of a specific View in the LowerViews list
update_lowerviews_state({View, ViewState}, LowerViews) -> 
	lists:keyreplace(View, 1, LowerViews, {View, ViewState, get_view_weight(View, LowerViews)}).
-ifdef(TEST).
update_lowerviews_state_test() ->
	LowerViews = [{"first", alive, 1}, {"myview", dead, 2}, {myview, alive, 1}],
	Result = [{"first", alive, 1}, {"myview", suscpicious, 2}, {myview, alive, 1}],
	Result = update_lowerviews_state({"myview", suscpicious}, LowerViews).
-endif.
%%

%% Update the weight of a specific View in the LowerViews list
update_lowerviews_weight({View, Weight}, LowerViews) ->
	lists:keyreplace(View, 1, LowerViews, {View, get_view_state(View, LowerViews), Weight}).
-ifdef(TEST).
update_lowerviews_weight_test() ->
	LowerViews = [{"first", alive, 1}, {"myview", dead, 2}, {myview, alive, 1}],
	Result = [{"first", alive, 1}, {"myview", dead, 3}, {myview, alive, 1}],
	Result = update_lowerviews_weight({"myview", 3}, LowerViews).
-endif.
%%

%% Get the weight of a View in the LowerViews list 
get_view_weight(View, LowerViews) ->
	{_, _, Weight} = lists:keyfind(View, 1, LowerViews),
	Weight.
-ifdef(TEST).
get_view_weight_test() ->
	LowerViews = [{"first", alive, 1}, {"myview", dead, 2}, {myview, alive, 1}],
	View = "myview",
	2 = get_view_weight(View, LowerViews).
-endif.
%%

%% Get the state of a View in the LowerViews list
get_view_state(View, LowerViews) ->
	{_, State, _} = lists:keyfind(View, 1, LowerViews),
	State.
-ifdef(TEST).
get_view_state_test() ->
	LowerViews = [{"first", alive, 1}, {"myview", dead, 2}, {myview, alive, 1}],
	View = "myview",
	dead = get_view_state(View, LowerViews).
-endif.
%%

%% Insertion sort on the second elements of a Tuple List 
inverted_insertion_sort(List) ->
	lists:foldl(fun inverted_insertion/2, [], List).
inverted_insertion(Acc, []) ->
	[Acc];
inverted_insertion(Acc={_, AccSecond}, List=[{_, Second}|_]) when AccSecond >= Second ->
	[Acc|List];
inverted_insertion(Acc,[H|T]) ->
	[H|inverted_insertion(Acc, T)].
-ifdef(TEST).
inverted_insertion_sort_test() ->
	Unsorted = [{3, 5}, {2, 6}, {1, 7}, {5, 1}],
	SortedSecond = [{1, 7}, {2, 6}, {3, 5}, {5, 1}],
	SortedSecond = inverted_insertion_sort(Unsorted).
-endif.
%%

%% Sum the weight of all the Views of a give State
sum_weights(State, ViewList) ->
	Sum = fun({_, X, Weight}, WeightSum) when X == State ->
				Weight + WeightSum;
			(_, WeightSum) ->
				WeightSum
		end,
	{State, lists:foldl(Sum, 0, ViewList)}.
-ifdef(TEST).
sum_weights_test() ->
	Views = [{first, alive, 1}, {second, alive, 3}, {third, suspicious, 2}, {fourth, dead, 2}],
	{alive, 4} = sum_weights(alive, Views),
	{dead, 2} = sum_weights(dead, Views).
-endif.
%%

%% Lists all the different State present inside the LowerViews list
list_states(Views) ->
	{_, States, _} = lists:unzip3(Views),
	%%sets:to_list(sets:from_list(States)).
	lists:usort(States).
-ifdef(TEST).
list_states_test() ->
	Views = [{first, alive, 1}, {second, alive, 3}, {third, suspicious, 2},     {fourth, dead, 2}],
	[alive, dead, suspicious] = list_states(Views).
-endif.
%%
