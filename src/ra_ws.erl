-module(ra_ws).
-author('Konstantin Kiselyov, <kiseljovkn@gmail.com>').

-export([
	start/2,
	stop/0
]).

start(NumClients, OutputDelay) ->
	ra_ws_monitor:start_link(OutputDelay),
	ra_ws_sup:start_link(),
	ra_ws_sup:start_clients(NumClients).

stop() ->
	ok.