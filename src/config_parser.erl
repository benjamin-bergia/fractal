-module(config_parser).
-export([get_config/1]).
-define(VIEW(N, E, T, O), {N, {view, start_link, [N, E, T, O]}, permanent, 5000, worker, [view]}).
-define(TIMEOUT, 5000).

get_config(File) ->
	{ok, Conf} = file:consult(File),
	parse(Conf, []).

parse([], Acc) ->
	Acc;
parse([{Name, Engine, Threshold, Tags}|T], Acc) ->
	parse(T, [?VIEW(Name, Engine, Threshold, Tags)|Acc]).
