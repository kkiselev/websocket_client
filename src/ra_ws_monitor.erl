-module(ra_ws_monitor).
-author('Konstantin Kiselyov, <kiseljovkn@gmail.com>').

-behaviour(gen_server).

-export([
	code_change/3,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	init/1,
	terminate/2
]).

-export([
	start_link/1,
	complete_request/2,
	start_request/1
]).

-record(client_state, {
	pid,
	total_requests = 0,
	completed_requests = 0,
	max_response_time = 0,
	min_response_time = 10000000000,
	avg_response_time = 0
}).

-record(monitor_state, {
	output_delay = 10000,
	clients_num = 0,
	total_requests = 0,
	completed_requests = 0,
	avg_response_time = 0,
	max_response_time = 0,
	min_response_time = 10000000000
}).

-define(CLIENTS_TABLE, clients_table).

%%% Gen server behaviour

init([OutputDelay | _]) ->
	ets:new(?CLIENTS_TABLE, [named_table, protected, set]),
	erlang:start_timer(OutputDelay, self(), <<"">>),
	{ok, #monitor_state{
		output_delay = OutputDelay
	}}.

handle_call(_Msg, _From, _State) ->
	{reply, undefined, _State}.

handle_cast({start_request, Pid}, State) ->
	{ClientState, State1} = get_state_for_pid(Pid, State),
	ClientState1 = ClientState#client_state{total_requests = ClientState#client_state.total_requests + 1},
	State2 = State1#monitor_state{total_requests = State1#monitor_state.total_requests + 1},
	save_state_for_pid(Pid, ClientState1),
	{noreply, State2};

handle_cast({complete_request, Pid, Time}, State) ->
	{ClientState, State1} = get_state_for_pid(Pid, State),
	#client_state {
		total_requests = TotalRequests,
		completed_requests = CompletedRequests,
		max_response_time = MaxResponseTime,
		min_response_time = MinResponseTime,
		avg_response_time = AvgResponseTime
	} = ClientState,

	CompletedRequests1 = CompletedRequests + 1,
	MaxResponseTime1 = erlang:max(MaxResponseTime, Time),
	MinResponseTime1 = erlang:min(MinResponseTime, Time),
	AvgResponseTime1 = AvgResponseTime * CompletedRequests / CompletedRequests1 + Time / CompletedRequests1, 

	ClientState1 = ClientState#client_state {
		completed_requests = CompletedRequests1,
		max_response_time = MaxResponseTime1,
		min_response_time = MinResponseTime1,
		avg_response_time = AvgResponseTime1
	},
	
	OldMonitorTotalCount = State1#monitor_state.completed_requests,
	NewMonitorTotalCount = OldMonitorTotalCount + 1,
	
	State2 = State1#monitor_state {
		completed_requests = NewMonitorTotalCount,
		max_response_time = erlang:max(State1#monitor_state.max_response_time, MaxResponseTime1),
		min_response_time = erlang:min(State1#monitor_state.min_response_time, MinResponseTime1),
		avg_response_time = case State1#monitor_state.clients_num of
			0 -> 0;
			ClientsNum -> 
				State1#monitor_state.avg_response_time * OldMonitorTotalCount / NewMonitorTotalCount + Time / NewMonitorTotalCount
		end
	},
	save_state_for_pid(Pid, ClientState1),
	{noreply, State2}.

handle_info({timeout, _Ref, Msg}, State) ->
	print_stats(State),
	erlang:start_timer(State#monitor_state.output_delay, self(), <<"">>),
	{noreply, State};

handle_info(_Msg, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, State) ->
	ok.

%%% Api methods

start_link(OutputDelay) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [OutputDelay], []).

start_request(Pid) ->
	gen_server:cast(?MODULE, {start_request, Pid}).

complete_request(Pid, Time) ->
	gen_server:cast(?MODULE, {complete_request, Pid, Time}).

%%% Helper methods

get_state_for_pid(Pid, MonitorState) ->
	case ets:lookup(?CLIENTS_TABLE, Pid) of
		[] -> 
			MonitorState1 = MonitorState#monitor_state{
				clients_num = MonitorState#monitor_state.clients_num + 1
			},
			{#client_state{pid = Pid}, MonitorState1};

		[{_, ClientState} | _] -> {ClientState, MonitorState}
	end.

save_state_for_pid(Pid, ClientState) ->
	ets:insert(?CLIENTS_TABLE, {Pid, ClientState}).

print_stats(State) ->
	io:format(
		"Statistics:\
			completed = ~p / ~p\
			avg response = ~p (s)\
			max response = ~p (s)\
			min response = ~p (s)\
			~n", 
			[
				State#monitor_state.completed_requests, 
				State#monitor_state.total_requests,
				State#monitor_state.avg_response_time/1000,
				State#monitor_state.max_response_time/1000,
				State#monitor_state.min_response_time/1000
			]),
	ok.










