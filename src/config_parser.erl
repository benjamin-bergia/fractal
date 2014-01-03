-module(config_parser).
-export([get_config/1, parse/1, parse/3]).

get_config(File) ->
	file:consult(File).

parse(Data) ->
	parse(Data, [], self()),
	lists:reverse(loop([])).
parse([{Name, []}], Upper, Pid) ->
	Pid ! {self(), {Name, Upper, []}};
parse([{Name, Lowers}], Upper, Pid) ->
	spawn(?MODULE, parse, [Lowers, [Name], Pid]),
	Pid ! {self(), {Name, Upper, get_names(Lowers)}};
parse([{Name, []}|T], Upper, Pid) ->
	spawn(?MODULE, parse, [T, Upper, Pid]),
	Pid ! {self(), {Name, Upper, []}};
parse([{Name, Lowers}|T], Upper, Pid) ->
	spawn(?MODULE, parse, [T, Upper, Pid]),
	spawn(?MODULE, parse, [Lowers, [Name], Pid]),
	Pid ! {self(), {Name, Upper, get_names(Lowers)}}.

get_names(ViewList) ->
	Fun = fun({ViewName, _L}, Acc) ->
			[ViewName|Acc]
		end,
	lists:foldl(Fun, [], ViewList).

loop(ViewList) ->
	receive
		{_From, {_Name, _Uppers, _Lowers}=View} ->
			loop([View|ViewList]);
		_Msg ->
			loop(ViewList)
	after
		10000 ->
			ViewList
	end.

