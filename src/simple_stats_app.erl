%%%-------------------------------------------------------------------
%% @doc simple_stats public API
%% @end
%%%-------------------------------------------------------------------

-module(simple_stats_app).

-behaviour(application).

-export([start/2, stop/1]).

%% ======================================================
%% API
%% ======================================================

start(_StartType, _StartArgs) ->
    lager:start(),
    application:ensure_all_started(exometer_core),
    http_handler:start_http_handler(),
    simple_stats_sup:start_link().

stop(_State) ->
    ok.