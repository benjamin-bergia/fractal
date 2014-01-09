-module(txer).
-export([start/2, send/2]).

start(To, Msg) ->
	spawn(?MODULE, send, [To, Msg]).

send(To, Msg) ->
	gen_server:call(To, Msg).
