-module(ra_ws_sup).
-behaviour(supervisor).

-export([
	start_link/0,
	start_client/0,
	start_clients/1,
	init/1
]).

start_link() ->
    crypto:start(),
    ssl:start(),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_client() ->
	{ok, ChildPid} = supervisor:start_child(?MODULE, []).

start_clients(NumClients) ->
	lists:foreach(
		fun (Index) ->
			start_client()
		end,
		lists:seq(0, NumClients - 1)
	).

client_spec() ->
	{
		ra_ws_handler_spec, 
		{ra_ws_handler, start_link, []},
		permanent,
		10,
		worker,
		[ra_ws_handler]
	}.

init(_Args) ->
	{ok, 
		{
			{simple_one_for_one, 100, 1},
			[client_spec()]
		}
	}.