-module(config_parser).
-export([get_config/1, parse/1, parse/3]).
-include("state.hrl").
-define(LOWER(N, W), {N, dead, W}).
-define(STATE(N, E, T, U, L), #state{view_name=N, threshold=T, engine=E, upper_views=U, lower_views=[?LOWER(view_sup, 1)|L]}).
-define(TIMEOUT, 5000).

get_config(File) ->
	{ok, Conf} = file:consult(File),
	to_view(parse(Conf)).

parse(Data) ->
	parse(Data, [], self()),
	loop([]).
parse([{_Weight, {Name, Engine, Threshold, []}}], Upper, Pid) ->
	Pid ! {self(), ?STATE(Name, Engine, Threshold, Upper, [])};
parse([{_Weight, {Name, Engine, Threshold, Lowers}}], Upper, Pid) ->
	spawn(?MODULE, parse, [Lowers, [Name], Pid]),
	Pid ! {self(), ?STATE(Name, Engine, Threshold,  Upper, get_lowers(Lowers))};
parse([{_Weight, {Name, Engine, Threshold, []}}|T], Upper, Pid) ->
	spawn(?MODULE, parse, [T, Upper, Pid]),
	Pid ! {self(), ?STATE(Name, Engine, Threshold, Upper, [])};
parse([{_Weight, {Name, Engine, Threshold, Lowers}}|T], Upper, Pid) ->
	spawn(?MODULE, parse, [T, Upper, Pid]),
	spawn(?MODULE, parse, [Lowers, [Name], Pid]),
	Pid ! {self(), ?STATE(Name, Engine, Threshold, Upper, get_lowers(Lowers))}.

get_lowers(LowerList) ->
	Fun = fun({Weight, {LowerName, _Engine, _Threshold, _Lowers}}, Acc) ->
			[?LOWER(LowerName, Weight)|Acc]
		end,
	lists:foldl(Fun, [], LowerList).

loop(StateList) ->
	receive
		{_From, State} ->
			loop([State|StateList]);
		_Msg ->
			loop(StateList)
	after
		?TIMEOUT ->
			StateList
	end.

to_view(StateList) ->
	Fun = fun(S, Acc) ->
			[?VIEW(S)|Acc]
		end,
	lists:foldl(Fun, [], StateList).
