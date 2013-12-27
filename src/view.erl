-module(view).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, create_state/1]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, alive/2, dead/2, suspicious/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

%% ------------------------------------------------------------------
%% State record
%% ------------------------------------------------------------------
-record(state, {view_name, engine, threshold, state_name, upper_views, lower_views}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(State) ->
    gen_fsm:start_link({local, State#state.view_name}, ?MODULE, State, []).

create_state({ViewName, Engine, Threshold, StateName, UpperViews, LowerViews}) ->
	#state{view_name=ViewName,
	       engine=Engine,
	       threshold=Threshold,
	       state_name=StateName,
	       upper_views=UpperViews,
	       lower_views=LowerViews}.

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(State) ->
    {ok, State#state.state_name, State}.

%%state_name(_Event, State) ->
%%    {next_state, state_name, State}.
%%
%%state_name(_Event, _From, State) ->
%%    {reply, ok, state_name, State}.

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

routine(Event, State) when is_record(State, state) ->
	NewState = update_state(Event, State),
	propagate(NewState),
	{next_state, NewState#state.state_name, NewState}.

%%update_state(Event,{Name, {Engine, Threshold}, StateName, UpperViews, LowerViews}) ->
update_state(Event, OldState) ->
	Current = update_lowerviews_state(Event, OldState),
	case Current#state.engine of  
		one_for_all ->
			%% In this case we don't care about the state of the other lowerviews
			NewState = one_for_all_engine(Current, Event);
		all_for_one ->
			NewState = all_for_one_engine(Current);
		weighted ->
			NewState = weighted_engine(Current)
	end,
	NewState.

propagate(State) ->
	Send = fun(Target) ->
			gen_fsm:send_event(Target, {State#state.view_name, State#state.state_name}),
			true
		end,
	lists:all(Send, State#state.upper_views).

one_for_all_engine(State, {_, ViewStateName}) ->
	case ViewStateName /= State#state.state_name of
		true ->
			State#state{state_name=ViewStateName};
		false ->
			State
	end.

all_for_one_engine(State) ->
	{_, [NewStateName|StateNames], _} = lists:unzip3(State#state.state_name),
	F = fun(X) when X == NewStateName ->
			    true;
	       (_) ->
			    false
		end,
	case lists:all(F, StateNames) of
		true ->
			State#state{state_name=NewStateName};
		false ->
			State
	end.

weighted_engine(State) ->
	States = list_statenames(State#state.lower_views),
	F = fun(S) ->
			sum_weights(S, State#state.lower_views)
		end,
	StateSums = lists:map(F, States),
	[{NewStateName, Sum}|[{_, Sum2}]] = inverted_insertion_sort(StateSums),
	if
		Sum < State#state.threshold ->
			State;
		Sum == Sum2 ->
			State#state{state_name=suspicious};
		Sum >= State#state.threshold ->
			State#state{state_name=NewStateName}
	end.

update_lowerviews_state({View, StateName}, State) ->
	UpdatedViews = lists:keyreplace(View, 1, State#state.lower_views, {View, StateName, get_weight(View, State#state.lower_views)}),
	State#state{lower_views=UpdatedViews}.	

get_weight(ViewName, LowerViews) ->
	{_, _, Weight} = lists:keyfind(ViewName, 1, LowerViews),
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

-ifdef(TEST).
-include("view_tests.hrl").
-endif.
