-include_lib("eunit/include/eunit.hrl").

%%start_link(Tid, DE, DT, AE, AT, SE, ST)
%%forward(From, Tid, DSum, ASum, SSum)
%%init({Tid, DE, DT, AE, AT, SE, ST})
start_engine_test() ->
	Status = dead,
	Engine = weighted_engine,
	Threshold = 3,
	StatusList = [{dead, 2}, {alive, 5}, {suspicious, 0}],
	?assertEqual(alive, start_engine(Status, Engine, Threshold, StatusList)).
