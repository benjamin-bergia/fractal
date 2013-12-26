-module(view).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, state_name/2, state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, initial_state_name, initial_state}.

state_name(_Event, State) ->
    {next_state, state_name, State}.

state_name(_Event, _From, State) ->
    {reply, ok, state_name, State}.

alive(Event, State) ->
	routine(Event, State).

dead(Event, State) ->
	routine(Event, State).

suspicious(Event, State) ->
	routine(Event, State).

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

routine(Event, State) ->
	{_, _, NewStateName, _, _} = NewState = update_state(Event, State),
	propagate(NewState),
	{next_state, NewStateName, NewState}.

update_state(Event,{Name, {Engine, Threshold}, StateName, UpperViews, LowerViews}) ->
	NewLowerViews = update_lowerviews_state(Event, LowerViews),
	case Engine of  
		one_for_all ->
			NewStateName = one_for_all_engine(StateName, Event);
		all_for_one ->
			NewStateName = all_for_one_engine(StateName, NewLowerViews);
		weighted ->
			NewStateName = weighted_engine(StateName, Threshold, NewLowerViews)
	end,
	{Name, {Engine, Threshold}, NewStateName, UpperViews, NewLowerViews}.

propagate({Name, _, StateName, UpperViews, _}) ->
	Send = fun(Target) ->
			gen_fsm:send_event(Target, {Name, StateName}),
			true
		end,
	lists:all(Send, UpperViews).

one_for_all_engine(StateName, {_, ViewStateName}) ->
	case ViewStateName /= StateName of
		true ->
			ViewStateName;
		false ->
			StateName
	end.

all_for_one_engine(StateName, LowerViews) ->
	{_, [NewStateName|StateNames], _} = lists:unzip3(LowerViews),
	F = fun(X) when X == NewStateName ->
			    true;
	       (_) ->
			    false
		end,
	case lists:all(F, StateNames) of
		true ->
			NewStateName;
		false ->
			StateName
	end.

weighted_engine(StateName, Threshold, LowerViews) ->
	States = list_statenames(LowerViews),
	F = fun(S) ->
			sum_weights(S, LowerViews)
		end,
	StateSums = lists:map(F, States),
	[{NewStateName, Sum}|[{_, Sum2}]] = inverted_insertion_sort(StateSums),
	if
		Sum < Threshold ->
			StateName;
		Sum == Sum2 ->
			suspicious;
		Sum >= Threshold ->
			NewStateName
	end.

update_lowerviews_state({View, StateName}, LowerViews) ->
	lists:keyreplace(View, 1, LowerViews, {View, StateName, get_weight(View, LowerViews)}).

get_weight(View, LowerViews) ->
	{_, _, Weight} = lists:keyfind(View, 1, LowerViews),
	Weight.

inverted_insertion_sort(List) ->
	lists:foldl(fun inverted_insertion/2, [], List).
inverted_insertion(Acc, []) ->
	[Acc];
inverted_insertion(Acc={_, AccSecond}, List=[{_, Second}|_]) when AccSecond >= Second ->
	[Acc|List];
inverted_insertion(Acc,[H|T]) ->
	[H|inverted_insertion(Acc, T)].

sum_weights(StateName, ViewList) ->
	Sum = fun({_, X, Weight}, WeightSum) when X == StateName ->
			Weight + WeightSum;
		(_, WeightSum) ->
			WeightSum
		end,
	{StateName, lists:foldl(Sum, 0, ViewList)}.

list_statenames(Views) ->
	{_, StateNames, _} = lists:unzip3(Views),
	lists:usort(StateNames).

