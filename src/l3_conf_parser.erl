-module(l3_conf_parser).
-export([parse/1]).

define(ROUTER, {router, 1})

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Call break for each view contained in the config file	
%% With:
%% 	FilePath: The path to the configuration file
%% @end
%%--------------------------------------------------------------------
parse(FilePath) ->
	[break(Conf) || Conf <- get_conf(FilePath)].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Get the views from the configuration file
%% With:
%% 	FilePath: The path to the configuration file
%% @end
%%--------------------------------------------------------------------
get_conf(FilePath) ->
	{ok, Conf} = file:consult(FilePath),
	Conf.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Go through the view structure and return the list of
%% 		information required to spawn the view
%% With:
%% 	ViewID:   The unique identifier of the view
%% 	ViewName: A non unique name for the view
%% 	Status:   Engine, Threshold and lowers for all the status (Simple View)
%% 	StatusA:  Engine, Threshold and lowers to use when dead
%% 	StatusB:  Engine, Threshold and lowers to use when dead
%% 	StatusC:  Engine, Threshold and lowers to use when dead
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Parse the Status structure
%% With:
%% 	Engine: The name of the engine to use 
%% 	Threshold: The threshold to use
%% 	Lowers: A list of all the lower views
%% @end
%%--------------------------------------------------------------------
get_status({all, _Engine, _Threshold, _Lowers}=Data) ->
	get_status_safe(Data);
get_status({dead, _Engine, _Threshold, _Lowers}=Data) ->
	get_status_safe(Data);
get_status({alive, _Engine, _Threshold, _Lowers}=Data) ->
	get_status_safe(Data);
get_status({suspicious, _Engine, _Threshold, _Lowers}=Data) ->
	get_status_safe(Data).
get_status_safe({_Status, {engine, Engine}, {threshold, Threshold}, Lowers}) ->
	case get_lowers(Lowers) of
		[?ROUTER] ->
			{Engine, 1, [?ROUTER]};
		_ ->
			{Engine, Threshold, get_lowers(Lowers)}
	end.
-ifdef(TEST).
get_status_safe_test_() ->
	Input = {dead, {engine, engine},
			{threshold, 2},
			{lowers, [{lower, {id, 1}, {weight, 1}}]}},
	?_assertEqual({engine, 2, [{1, 1}]}, get_status(Input)).
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do:
%% 	Go through the list of lowers and extract the ID and Weight
%% 		of each of them
%% With:
%% 	LowerList: A list lowers
%% @end
%%--------------------------------------------------------------------
get_lowers({lowers, []}) ->
	[?ROUTER];	% If the view has no lowers, we connect it to the router
get_lowers({lowers, LowersList}) ->
	F = fun({lower, {id, ID}, {weight, Weight}}, Acc) ->
			[{ID, Weight}|Acc]
		end,
	lists:foldl(F, [], LowersList).
-ifdef(TEST).
get_lowers_test_() ->
	?_assertEqual([{3, 4}, {1, 2}], get_lowers({lowers, [{lower, {id, 1}, {weight, 2}}, {lower, {id, 3}, {weight, 4}}]})).
-endif.
