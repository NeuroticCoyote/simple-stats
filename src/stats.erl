%%%-------------------------------------------------------------------
%%% @author joegoodwin
%%% @copyright (C) 2021
%%% @doc
%%%
%%% @end
%%% Created : 06. Jan 2021 14:09
%%%-------------------------------------------------------------------
-module(stats).
-author("joegoodwin").

-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/0, update_stats/1, get_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).
-define(INCREMENT_SCHEDULED_REPORT_COUNT(), simple_stats:update_counter([scheduled, reports], 1)).

-record(stats, {stats=#{}, tcp_socket}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

update_stats(Stat) ->
	gen_server:cast(?SERVER, {update_stats, Stat}).

get_stats() ->
	gen_server:call(?SERVER, get_stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
	UpdatedState = connect_tcp(),
	schedule_report(),
	{ok, UpdatedState}.

handle_call({update_state, State}, _From, _State) ->
	{reply, ok, State};
handle_call(get_stats, _From, State = #stats{stats = StatsMap}) ->
	{reply, StatsMap, State};
handle_call(_Request, _From, State = #stats{}) ->
	{reply, ok, State}.

handle_cast({update_stats, {StatKey, {ok, Stat}}}, State = #stats{stats = StatsMap}) ->
	UpdatedStatsMap = maps:put(list_to_tuple(StatKey), Stat, StatsMap),
	{noreply, State#stats{stats = UpdatedStatsMap}};
handle_cast(_Request, State = #stats{}) ->
	{noreply, State}.

handle_info(report, State) ->
 	format_and_send_state(State),
	schedule_report(),
	{noreply, State};
handle_info(_Info, State = #stats{}) ->
	{noreply, State}.

terminate(_Reason, _State = #stats{}) ->
	ok.

code_change(_OldVsn, State = #stats{}, _Extra) ->
	{ok, State}.

connect_tcp() ->
	case gen_tcp:connect(localhost, 10001, [binary, {packet, 0}, {active, false}]) of
		{ok, Socket} ->
			lager:info("Connected to localhost on port 10001..."),
			#stats{tcp_socket = Socket};
		{error, _} = Error ->
			lager:error("Error connecting to localhost, retrying in 2 seconds. Error: ~p~n", [Error]),
			timer:sleep(2000),
			connect_tcp()
	end.

schedule_report() ->
	?INCREMENT_SCHEDULED_REPORT_COUNT(),
	erlang:send_after(10000, self(), report).

format_and_send_state(#stats.stats = {}) ->
	ok;
format_and_send_state(#stats.tcp_socket = undefined) ->
	ok;
format_and_send_state(#stats{stats = Stats, tcp_socket = Socket}) ->
	lager:info("Formatting and sending report to logstash on port 10002..."),
	FormattedStats =
		maps:fold(fun(K,V,AccIn) ->
			lists:foldl(fun({K1,V1}, Acc) ->
					FormattedKey = lists:foldr(fun(E2, Acc2) -> <<(atom_to_binary(E2))/binary, ".",  Acc2/binary>> end, <<"">>, tuple_to_list(K)),
					FormattedKey1 = <<FormattedKey/binary, (atom_to_binary(K1))/binary>>,
					maps:put(FormattedKey1, V1, Acc)
				end, AccIn, V)
			end, #{}, Stats),
	JSON = jsx:encode(FormattedStats),
	ok = gen_tcp:send(Socket, <<JSON/binary, "\n">>).
