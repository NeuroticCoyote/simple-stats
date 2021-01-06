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

%% API
-export([start_link/0, update_stats/1, get_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(stats, {stats=#{}}).

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
	{ok, #stats{}}.

handle_call(get_stats, _From, State = #stats{stats = StatsMap}) ->
	{reply, StatsMap, State};
handle_call(_Request, _From, State = #stats{}) ->
	{reply, ok, State}.

handle_cast({update_stats, {StatKey, {ok, Stat}}}, State = #stats{stats = StatsMap}) ->
	UpdatedStatsMap = maps:put(list_to_tuple(StatKey), Stat, StatsMap),
	{noreply, State#stats{stats = UpdatedStatsMap}};
handle_cast(_Request, State = #stats{}) ->
	{noreply, State}.

handle_info(_Info, State = #stats{}) ->
	{noreply, State}.

terminate(_Reason, _State = #stats{}) ->
	ok.

code_change(_OldVsn, State = #stats{}, _Extra) ->
	{ok, State}.