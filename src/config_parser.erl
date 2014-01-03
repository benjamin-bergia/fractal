-module(config_parser).
-export([get_config/1, parse/1, parse/3]).
-include("state.hrl").
-define(LOWER(N), {N, dead, 1}).
-define(STATE(N, U, L), #state{view_name=N, upper_views=U, lower_views=L}).
-define(TIMEOUT, 3000).

get_config(File) ->
	{ok, Conf} = file:consult(File),
	to_view(parse(Conf)).

parse(Data) ->
	parse(Data, [], self()),
	loop([]).
parse([{Name, []}], Upper, Pid) ->
	Pid ! {self(), ?STATE(Name, Upper, [])};
parse([{Name, Lowers}], Upper, Pid) ->
	spawn(?MODULE, parse, [Lowers, [Name], Pid]),
	Pid ! {self(), ?STATE(Name, Upper, get_lowers(Lowers))};
parse([{Name, []}|T], Upper, Pid) ->
	spawn(?MODULE, parse, [T, Upper, Pid]),
	Pid ! {self(), ?STATE(Name, Upper, [])};
parse([{Name, Lowers}|T], Upper, Pid) ->
	spawn(?MODULE, parse, [T, Upper, Pid]),
	spawn(?MODULE, parse, [Lowers, [Name], Pid]),
	Pid ! {self(), ?STATE(Name, Upper, get_lowers(Lowers))}.

get_lowers(ViewList) ->
	Fun = fun({ViewName, _L}, Acc) ->
			[?LOWER(ViewName)|Acc]
		end,
	lists:foldl(Fun, [], ViewList).

loop(ViewList) ->
	receive
		{_From, View} ->
			loop([View|ViewList]);
		_Msg ->
			loop(ViewList)
	after
		?TIMEOUT ->
			ViewList
	end.

to_view(StateList) ->
	Fun = fun(S, Acc) ->
			[?VIEW(S)|Acc]
		end,
	lists:foldl(Fun, [], StateList).
