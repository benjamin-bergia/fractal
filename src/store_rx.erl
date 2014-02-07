-module(store_rx).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% State record
%% ------------------------------------------------------------------

-record(view_status, {view_id, status}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, set_status/2, install/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_cast/2, terminate/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_status(ViewID, Status) ->
	gen_server:cast(?MODULE, {set_status, ViewID, Status}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
	{ok, []}.

handle_cast({set_status, ViewID, Status}, S) ->
	write(#view_status{view_id=ViewID, status=Status}),
	{noreply, S}.

terminate(_Reason, _State) ->
	ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

write(Record) ->
	Write = fun() ->
			mnesia:write(Record)
		end,
	mnesia:activity(sync_transaction, Write).


install() ->
	ok = mnesia:create_schema([node()]),
	application:start(mnesia),
	mnesia:create_table(view_status, [{type, set}, {attributes, record_info(fields, view_status)}]),
	application:stop(mnesia).
