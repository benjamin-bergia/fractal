-module(storage).
-behaviour(gen_server).
-include("state.hrl").
-define(SERVER, ?MODULE).
-define(DB, "/tmp/mnesia").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_cast/2, terminate/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	start(),
    	{ok, Args}.

handle_cast({set, state, ViewState}, State) ->
	set_state(ViewState),
    	{noreply, State}.

terminate(_Reason, _State) ->
	stop(),
    	ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start() ->
	application:set_env(mnesia, dir, ?DB),
	mnesia:create_schema([node()]),
	application:start(mnesia),
	mnesia:create_table(state, [{attributes, record_info(fields, state)}]).

stop() ->
	application:stop(mnesia).

set_state(State) ->
	writer:start(State).
