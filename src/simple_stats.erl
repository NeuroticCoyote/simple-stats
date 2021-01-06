%%%-------------------------------------------------------------------
%%% @author joegoodwin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jul 2021 14:43
%%%-------------------------------------------------------------------
-module(simple_stats).
-author("joegoodwin").

%% API
-export([update_counter/2, update_gauge/2, update_spiral/2, update_histogram/2]).

update_counter(Name, Integer) ->
	update_stat(Name, counter, Integer).

update_gauge(Name, Integer) ->
	update_stat(Name, gauge, Integer).

update_spiral(Name, Integer) ->
	update_stat(Name, spiral, Integer).

update_histogram(Name, Integer) ->
	update_stat(Name, histogram, Integer).

%% name needs a list such as [counter, name, here]
%% type can be: gauge, meter, spiral, histogram etc
%% e.g update_counter([basic, counter], gauge, 100)
update_stat(Name, Type, Value) ->
	case exometer:update(Name, Value) of
		{error, not_found} ->
			exometer:new(Name, Type),
			exometer:update(Name, Value),
			Stat = get_stat(Name),
			stats:update_stats({Name, Stat});
		_ ->
			Stat = get_stat(Name),
			stats:update_stats({Name, Stat})
	end.

get_stat(Name) ->
	DataPoints = exometer:info(Name, datapoints),
	exometer:get_value(Name, DataPoints).