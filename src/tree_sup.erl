-module(tree_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(VIEW(ViewID, Options), {ViewID, {view_sup, start_link, Options}, permanent, 5000, supervisor, [view_sup]}).
-define(PARSER, l3_conf_parser).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Fun = fun(Options, Acc) ->
			      [?VIEW(get_viewid(Options), Options)|Acc]
			end,
	Childs = lists:foldr(Fun, [], ?PARSER:parse("priv/multiView.hrl")),
	{ok, {{one_for_one, 5, 10}, Childs}}.

get_viewid(Options) ->
	[ViewID| _T] = Options,
	ViewID.
