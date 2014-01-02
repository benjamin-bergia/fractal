-module(state_store).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(STORAGE, storage).

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
    	{ok, Args}.

handle_cast({set_state, ViewState}, State) ->
	set_state(ViewState),
    	{noreply, State}.

terminate(_Reason, _State) ->
    	ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

set_state(State) ->
	ok = gen_server:cast(?STORAGE, {set, state, State}).
