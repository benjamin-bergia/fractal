-module(engine).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([process/4]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Do:
%% 	Send a synchronous request to the engine
%% With:
%% 	Engine: the engine to use
%% 	Status: current status of the calling view_core
%% 	Threshold: threshold to use
%% 	StatusList: list of 2-Tuples
%% @end
%%--------------------------------------------------------------------
process(Engine, Status, Threshold, StatusList) ->
	Engine:process(Status, Threshold, StatusList).
