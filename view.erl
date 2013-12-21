-module(view_fsm).
-behaviour(gen_fsm).

-export([start/1]).

%%start(StateData) ->
%%	gen_fsm:start_link({local, view}, view_fsm, StateData, []).

%%init(StateData) ->
%%	{ok, State, StateData}.

%%alive(Event, StateData) ->
%%	{next_state, NewState, NewStateData}.

%%suspicious(Event, StateData) ->
%%	{next_state, NewState, NewStateData}.

%%dead(Event, StateData) ->
%%	{next_state, NewState, NewStateData}.

%%handle_event(stop, _StateName, StateData) ->
%%	{stop, normal, StateData}.

%% Event
%% {ViewName, ViewState}

%% StateData
%% {Name, Policy, State, UpperViews, LowerViews}

%% UpperViews
%% [Name|_]

%% LowerViews
%% [{Name, Status, Weight}|_]

%% Type
%% one LowerView down == State Change
%% all LowerView down == State Change
%% Weighted ...

%% Policy
%% {Type, Threshold}

start(StateData) ->
	gen_fsm:start({local, view}, view_fsm, StateData, []).

init(StateData) ->
	{ok, dead, StateData}.

alive(Message, StateData) ->
	routine(Message, StateData).
dead(Message, StateData) ->
	routine(Message, StateData).
suspicious(Message, StateData) ->
	routine(Message, StateData).

routine(Message, StateData) ->
	NewStateData = check_status(Message, StateData),
	propagate(NewStateData),
	{_, _, NextState, _, _} = NewStateData,
	{next_state, NextState, StateData}.
	
propagate(NewStateData) ->
	{Name, _, State, UpperViews, _} = NewStateData,
	[ gen_fsm:send_event(UpperView, {Name, State}) | UpperView <- UpperViews ].

check_status(Message, StateData) ->
	{Name, Policy, State, UpperViews, LowerViews} = StateData,
	case Policy of 
		{one_for_all, _} ->
			NewState = one_for_all_engine(State, Message);
		{all_for_one, _} ->
			NewState = all_for_one_engine(State, Message, LowerViews);
		{weighted, Threshold} ->
			NewState = weighted_engine(Threshold, State, Message, LowerViews);
	end,
	
	{Name, Policy, NewState, UpperViews, update_lowerviews_state(Message, LowerViews)}.

%% one_for_all: State changes when at least one of the lower Views has a different State
one_for_all_engine(State, {View, ViewState}) ->
	case ViewState != State of 
		true ->
			ViewState;
		false ->
			State
	end.

%% all_for_one: State changes when all the lower Views have the same State
all_for_one_engine(State, {View, ViewState}, LowerViews) -> 
	.

update_lowerviews_state({View, ViewState}, LowerViews) -> 
	lists:keyreplace(View, 1, {View, ViewState, get_view_weight(View, LowerViews)}).

update_lowerviews_weight({View, Weight}, LowerViews) ->
	lists:keyreplace(View, 1, {View, get_view_state(View, LowerViews), Weight}).

get_view_weight(View, LowerViews) ->
	{_, _, Weight} = lists:keyfind(View, 1, LowerViews),
	Weight.

get_view_state(View, LowerViews) ->
	{_, State, _} = lists:keyfind(View, 1, LowerViews),
	State.
