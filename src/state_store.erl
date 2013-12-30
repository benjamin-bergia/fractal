-module(state_store).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(DB, "/tmp/mnesia").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start/0, stop/0, install/0, set_state/1, get_state/1]).

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

handle_call(Request, _From, State) ->
	case Request of
		{set, ViewState} ->
			set_state(ViewState);
		{get, ViewName} ->
			get_state(ViewName)
	end,
    	{reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
	stop(),
    	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start() ->
	application:set_env(mnesia, dir, ?DB),
	application:start(mnesia).

stop() ->
	application:stop(mnesia).
 
install() ->
	application:set_env(mnesia, dir, ?DB),
	mnesia:create_schema([node()]),
	application:start(mnesia),
	%%mnesia:create_table(state, [{disc_copies, [node()]}, {ram_copies, [node()]}, {type, set}, {attributes, record_info(fields, state)}, {index, [view_name]}]),
	mnesia:create_table(state, [{attributes, view:generate_fields()}, {disc_copies ,nodes()}]).

set_state(State) ->
	Write = fun() ->
			mnesia:write(State)
		end,
	mnesia:transaction(Write).

get_state(ViewName) ->
	Read = fun() ->
			mnesia:read({state, ViewName})
		end,
	mnesia:transaction(Read).

