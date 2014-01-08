-module(txer).
-export([start/2]).

start(To, Msg) ->
	spawn(?MODULE, send, [To, Msg]).

send(To, Msg) ->
	{ack, From} = gen_server:call(To, Msg),
	true.
