-module(view_sup).
-behaviour(supervisor).

-export([]).

start_link() ->
	supervisor:start_link(view_sup, []).


