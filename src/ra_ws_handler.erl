-module(ra_ws_handler).

-behaviour(websocket_client_handler).

-export([
         start_link/0,
         init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

-record(state, {
	last_request_start = {0, 0, 0}
}).

start_link() ->
    websocket_client:start_link("ws://localhost:8085/ws", ?MODULE, []).

now_to_seconds() ->
    {Mega, Sec, _} = erlang:now(),
    (Mega * 1000000) + Sec.

now_to_ms() ->
	{Mega, Sec, Micro} = erlang:now(),
	((Mega * 1000000) + Sec) * 1000000 + Micro.

ms_from_time({Mega, Sec, Micro}) ->
	{Me, Se, Mi} = erlang:now(),
	(((Me - Mega)*1000000 + (Se - Sec))*1000000 + (Mi - Micro))/1000.


create_new_message() ->
	UniquePart = <<"1a2s3d4f5g6h7j8k9l">>,
	TsPart = <<(list_to_binary(integer_to_list(now_to_seconds())))/binary>>,
	ActionPart = <<"main/index.json">>,
	MethodPart = <<"GET">>,
	QueryStrPart = <<"">>,
	ParamsPart = <<"{}">>,
	<<"{",
		"\"unique\":", 		"\"", UniquePart/binary, 	"\",",
		"\"ts\":", 				  TsPart/binary, 		",",
		"\"action\":", 		"\"", ActionPart/binary, 	"\",",
		"\"method\":", 		"\"", MethodPart/binary,	"\",",
		"\"query_str\":", 	"\"", QueryStrPart/binary, 	"\",",
		"\"params\":", 			  ParamsPart/binary,
	"}">>.


init([], _ConnState) ->
	Message = create_new_message(),
    websocket_client:cast(self(), {text, Message}),
    ra_ws_monitor:start_request(self()), 
    {ok, #state {last_request_start = erlang:now()}}.

websocket_handle({pong, _}, _ConnState, State) ->
    {ok, State};

% websocket_handle({text, Msg}, _ConnState, 5) ->
%     io:format("Received msg:~n~p~n", [Msg]),
%     {close, <<>>, "done"};

websocket_handle({text, Msg}, _ConnState, State) ->
	LastStartTime = State#state.last_request_start,
    ra_ws_monitor:complete_request(self(), ms_from_time(LastStartTime)),
    timer:sleep(5000),
    BinInt = list_to_binary(integer_to_list(State)),
    Message = create_new_message(),
    ra_ws_monitor:start_request(self()),
    State1 = State#state{last_request_start = erlang:now()},
    {reply, {text, Message}, State1}.

websocket_info(start, _ConnState, State) ->
    {reply, {text, <<"erlang message received">>}, State}.

websocket_terminate(Reason, _ConnState, State) ->
    io:format("Websocket closed in state ~p wih reason ~p~n", [State, Reason]),
    ok.
