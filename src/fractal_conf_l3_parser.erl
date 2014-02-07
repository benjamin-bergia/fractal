-module(fractal_conf_l3_parser).
-export([parse/1]).

parse(FilePath) ->
	[break(Conf) || Conf <- get_conf(FilePath)].

get_conf(FilePath) ->
	{ok, Conf} = file:consult(FilePath),
	Conf.

break({view, {id, ViewID}, {name, _ViewName}, Status}) ->
	{E, T, L} = get_status(Status),
	[ViewID, E, T, L, E, T, L, E, T, L];
break({view, {id, ViewID}, {name, _ViewName}, StatusA, StatusB, StatusC}) ->
	{DE, DT, DL} = get_status(StatusA),
	{AE, AT, AL} = get_status(StatusB),
	{SE, ST, SL} = get_status(StatusC),
	[ViewID, DE, DT, DL, AE, AT, AL, SE, ST, SL].

get_status({all, {engine, Engine}, {threshold, Threshold}, Lowers}) ->
	{Engine, Threshold, get_lowers(Lowers)};
get_status({dead, {engine, Engine}, {threshold, Threshold}, Lowers}) ->
	{Engine, Threshold, get_lowers(Lowers)};
get_status({alive, {engine, Engine}, {threshold, Threshold}, Lowers}) ->
	{Engine, Threshold, get_lowers(Lowers)};
get_status({suspicious, {engine, Engine}, {threshold, Threshold}, Lowers}) ->
	{Engine, Threshold, get_lowers(Lowers)}.

get_lowers({lowers, []}) ->
	[];
get_lowers({lowers, LowersList}) ->
	F = fun({lower, {id, ID}, {weight, Weight}}, Acc) ->
			[{ID, Weight}|Acc]
		end,
	lists:foldl(F, [], LowersList).
