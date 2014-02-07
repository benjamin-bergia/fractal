-module(l1_conf_parser).
-export([parse/1]).

parse(FilePath) ->
	%break(get_conf(FilePath)).
	ok.

get_conf(FilePath) ->
	{ok, Conf} = file:consult(FilePath),
	Conf.

uppers_to_lowers(Lower, Uppers) ->
	uppers_to_lowers(Lower, Uppers, []).
uppers_to_lowers(Lower, [], Acc) ->
	[{Lower, []}|Acc];
uppers_to_lowers(Lower, [Upper|T], Acc) ->
	uppers_to_lowers(Lower, T, [{Upper, [Lower]}|Acc]).

update_lowers(Upper, Lower) ->
	ok.

