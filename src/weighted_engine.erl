-module(weighted_engine).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/3, terminate/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Status, Threshold, StatusList) ->
	[First|[Second|_T]] = inverted_insertion_sort(StatusList),
	{stop, compare(Status, Threshold, First, Second)}.

terminate(_Reason, Status) ->
	{ok, Status}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

inverted_insertion_sort(List) ->
	lists:foldl(fun inverted_insertion/2, [], List).
inverted_insertion(Acc, []) ->
	[Acc];
inverted_insertion(Acc={_, AccSecond}, List=[{_, Second}|_]) when AccSecond >= Second ->
	[Acc|List];
inverted_insertion(Acc,[H|T]) ->
	[H|inverted_insertion(Acc, T)].

compare(_Status, Threshold, {_StatusA, Sum}, {_StatusB, Sum}) when Sum >= Threshold ->
	suspicious;
compare(_Status, Threshold, {StatusA, Sum}, _) when Sum >= Threshold ->
	StatusA;
compare(Status, _, _, _) ->
	Status.

-ifdef(TEST).
-include("test/weighted_engine_tests.hrl").
-endif.
