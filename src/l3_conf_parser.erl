-module(l3_conf_parser).
-export([parse/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

parse(FilePath) ->
	[break(Conf) || Conf <- get_conf(FilePath)].

get_conf(FilePath) ->
	{ok, Conf} = file:consult(FilePath),
	Conf.

break({view, {id, ViewID}, {name, _ViewName}, Status}) ->
	{E, T, L} = get_status(Status),
	[ViewID, E, T, L];
break({view, {id, ViewID}, {name, _ViewName}, StatusA, StatusB, StatusC}) ->
	{DE, DT, DL} = get_status(StatusA),
	{AE, AT, AL} = get_status(StatusB),
	{SE, ST, SL} = get_status(StatusC),
	[ViewID, DE, DT, DL, AE, AT, AL, SE, ST, SL].
-ifdef(TEST).
break_test_() ->
	SimpleView = {view,
		      {id, 1},
		      {name, "nothing"},
		      {all,
		       {engine, engine},
		       {threshold, 2},
		       {lowers, [{lower,
				  {id, 2},
				  {weight, 3}}]}}},
	ComplexView = {view,
		      {id, 1},
		      {name, "nothing"},
		      {dead,
		       {engine, engine},
		       {threshold, 2},
		       {lowers, [{lower,
				  {id, 2},
				  {weight, 3}}]}},
		      {alive,
		       {engine, engine},
		       {threshold, 2},
		       {lowers, []}},
		      {suspicious,
		       {engine, engine},
		       {threshold, 2},
		       {lowers, [{lower,
				  {id, 2},
				  {weight, 3}}]}}},

	[?_assertEqual([1, engine, 2, [{2, 3}]],
		       break(SimpleView)),
	 ?_assertEqual([1, engine, 2, [{2, 3}],
				engine, 2, [],
				engine, 2, [{2, 3}]],
		       break(ComplexView))].
-endif.

get_status({all, _Engine, _Threshold, _Lowers}=Data) ->
	get_status_safe(Data);
get_status({dead, _Engine, _Threshold, _Lowers}=Data) ->
	get_status_safe(Data);
get_status({alive, _Engine, _Threshold, _Lowers}=Data) ->
	get_status_safe(Data);
get_status({suspicious, _Engine, _Threshold, _Lowers}=Data) ->
	get_status_safe(Data).
get_status_safe({_Status, {engine, Engine}, {threshold, Threshold}, Lowers}) ->
	{Engine, Threshold, get_lowers(Lowers)}.
-ifdef(TEST).
get_status_safe_test_() ->
	Input = {dead, {engine, engine},
			{threshold, 2},
			{lowers, [{lower, {id, 1}, {weight, 1}}]}},
	?_assertEqual({engine, 2, [{1, 1}]}, get_status(Input)).
-endif.

get_lowers({lowers, []}) ->
	[];
get_lowers({lowers, LowersList}) ->
	F = fun({lower, {id, ID}, {weight, Weight}}, Acc) ->
			[{ID, Weight}|Acc]
		end,
	lists:foldl(F, [], LowersList).
-ifdef(TEST).
get_lowers_test_() ->
	?_assertEqual([{3, 4}, {1, 2}], get_lowers({lowers, [{lower, {id, 1}, {weight, 2}}, {lower, {id, 3}, {weight, 4}}]})).
-endif.
