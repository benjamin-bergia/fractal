-module(writer).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/1]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, terminate/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start(Record) ->
    gen_fsm:start(?MODULE, Record, []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(Record) ->
	mnesia:transaction(fun() -> mnesia:write(Record) end),
    	{stop, normal, Record}.

terminate(normal, _StateName, _State) ->
    ok.
