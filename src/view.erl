-module(view).
-behaviour(gen_server).
-define(STATESTORE, state_store).
%% Local Name tuple for gproc
-define(GP_NL(N), {n,l,N}). 
%% Main loop Timeout
-define(TIMEOUT, 5000).

%% Import the state record
-include("state.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(State) when is_record(State, state) ->
    gen_server:start_link(?MODULE, State, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(State) ->
	gproc:reg(?GP_NL(State#state.view_name)),
    	{ok, State#state{state_name=dead}, ?TIMEOUT}. % Default state at initialization should be dead

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(Msg, OldState) ->
	State = select_engine(Msg, OldState),
	propagate(State),
    	{noreply, State}.

handle_info(timeout, State) ->
	propagate(State),
	{noreply, State}.	

terminate(_Reason, _State) ->
    	ok.

code_change(_OldVsn, State, _Extra) ->
    	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% This function call the engine specified in state record
select_engine(Msg, OldState) ->
	Current = update_lowerviews_state(Msg, OldState),
	case Current#state.engine of  
		one_for_all ->
			% In this case we don't care about the state of the other lowerviews
			NewState = one_for_all_engine(Current, Msg);
		all_for_one ->
			NewState = all_for_one_engine(Current);
		weighted ->
			NewState = weighted_engine(Current)
	end,
	NewState.

%% Send the new state_name to the upper views
propagate(State) ->
	gen_server:cast(?STATESTORE, {set_state, State}), % First update the state in mnesia 
	case State#state.upper_views of
		[] ->
			true;
		_ ->
			Msg = {state_update, State#state.view_name, State#state.state_name},
			Send = fun(Target) ->
					Pid = gproc:lookup_pid(?GP_NL(Target)),
					gen_server:cast(Pid, Msg),
					true
				end,
			lists:all(Send, State#state.upper_views)
		
	end.

%% First possible engine: when the sate_name received from the lowerview is different that the current one, the state_name is changed
one_for_all_engine(State, {state_update, _ViewName, ViewStateName}) ->
	case ViewStateName /= State#state.state_name of
		true ->
			State#state{state_name=ViewStateName};
		false ->
			State
	end.

%% The state_name change if all the lower_views have the a state_name different that the current one. 
all_for_one_engine(State) ->
	{_, [NewStateName|StateNames], _} = lists:unzip3(State#state.lower_views),
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

%% Change the state_name if the some of th weights of the lower_views sharing a same state_name is greater than threshold
weighted_engine(State) ->
	States = list_statenames(State#state.lower_views),
	F = fun(S) ->
			sum_weights(S, State#state.lower_views)
		end,
	StateSums = lists:map(F, States),
	Sorted = inverted_insertion_sort(StateSums),
	case Sorted of
		[{NewStateName, Sum}|[{_StateName, Sum2}]] -> 
			if
				Sum < State#state.threshold ->
					State;
				Sum == Sum2 ->
					State#state{state_name=suspicious};
				Sum >= State#state.threshold ->
					State#state{state_name=NewStateName}
			end;
		[{NewStateName, Sum}] ->
			if
				Sum < State#state.threshold ->
					State;
				Sum >= State#state.threshold ->
					State#state{state_name=NewStateName}
			end
	end.

%% Update the lower_viewis
update_lowerviews_state({state_update, ViewName, StateName}, State) ->
	ViewWeight = get_weight(ViewName, State#state.lower_views),
	UpdatedViews = lists:keyreplace(ViewName, 1, State#state.lower_views, {ViewName, StateName, ViewWeight}),
	State#state{lower_views=UpdatedViews}.	

%% Return the weight of a specific lower view
get_weight(ViewName, LowerViews) ->
	{_ViewName, _StateName, Weight} = lists:keyfind(ViewName, 1, LowerViews),
	Weight.

%% Sort a list of 2-Tuples using the second element of the tuple 
inverted_insertion_sort(List) ->
	lists:foldl(fun inverted_insertion/2, [], List).
inverted_insertion(Acc, []) ->
	[Acc];
inverted_insertion(Acc={_, AccSecond}, List=[{_, Second}|_]) when AccSecond >= Second ->
	[Acc|List];
inverted_insertion(Acc,[H|T]) ->
	[H|inverted_insertion(Acc, T)].

%% Sum the weights of all the lower views in a specific state_name
sum_weights(StateName, ViewList) ->
	Sum = fun({_ViewName, X, Weight}, WeightSum) when X == StateName ->
			Weight + WeightSum;
		(_, WeightSum) ->
			WeightSum
		end,
	{StateName, lists:foldl(Sum, 0, ViewList)}.

%% List the different state_name present in lower_views
list_statenames(Views) ->
	{_ViewNames, StateNames, _Weights} = lists:unzip3(Views),
	lists:usort(StateNames).

%% Include the unit tests 
-ifdef(TEST).
-include("view_tests.hrl").
-endif.
