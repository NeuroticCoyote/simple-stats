%%%-------------------------------------------------------------------
%% @doc simple_stats top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(simple_stats_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        #{
            id => child_id,
            start => {stats, start_link, []},
            restart => permanent,
            type => worker,
            modules => [stats]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.