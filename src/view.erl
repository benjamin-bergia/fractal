-module(view).
-define(SUP, view_sup).
-define(RX, view_rx).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/8, notify/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(ViewName, Lowers, DE, DT, AE, AT, SE, ST) ->
	?SUP:start_link(ViewName, Lowers, DE, DT, AE, AT, SE, ST).

notify(From, Status) ->
	?RX:forward(From, Status).
