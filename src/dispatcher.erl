-module(dispatcher).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_cast/2, terminate/2, notify/2]).

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

handle_cast({set, state_name, ViewName, StateName}, State) ->
	dispatch_update(ViewName, StateName),
    	{noreply, State}.

terminate(_Reason, _State) ->
    	ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

dispatch_update(ViewName, StateName) ->
	Fun = fun() ->
			view:notify(ViewName, ?SERVER, StateName)
		end,
	spawn(Fun).

notify(ViewName, StateName) ->
	gen_server:cast(?SERVER, {set, state_name, ViewName, StateName}).
