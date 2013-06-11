-module(ra_ws_handler).

-behaviour(websocket_client_handler).

-export([
         start_link/0,
         init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

start_link() ->
    websocket_client:start_link("ws://localhost:8085/ws", ?MODULE, []).

now_to_seconds({Mega, Sec, _}) ->
    (Mega * 1000000) + Sec.

create_new_message() ->
	UniquePart = <<"1a2s3d4f5g6h7j8k9l">>,
	TsPart = <<(list_to_binary(integer_to_list(now_to_seconds(erlang:now()))))/binary>>,
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
    {ok, 2}.

websocket_handle({pong, _}, _ConnState, State) ->
    {ok, State};

% websocket_handle({text, Msg}, _ConnState, 5) ->
%     io:format("Received msg:~n~p~n", [Msg]),
%     {close, <<>>, "done"};

websocket_handle({text, Msg}, _ConnState, State) ->
    % io:format("Received msg:~n~p~n", [Msg]),
    timer:sleep(1000),
    BinInt = list_to_binary(integer_to_list(State)),
    Message = create_new_message(),
    {reply, {text, Message}, State + 1}.

websocket_info(start, _ConnState, State) ->
    {reply, {text, <<"erlang message received">>}, State}.

websocket_terminate(Reason, _ConnState, State) ->
    io:format("Websocket closed in state ~p wih reason ~p~n", [State, Reason]),
    ok.
