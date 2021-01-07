%%%-------------------------------------------------------------------
%% @doc simple_stats public API
%% @end
%%%-------------------------------------------------------------------

-module(simple_stats_app).

-behaviour(application).

-export([start/2, stop/1, receiver/1]).
-export([update_gauge/2, update_spiral/2, update_histogram/2, update_stat/3, get_stat/1]).

-define(INCREMENT_HTTP_RECEIVED_GET_COUNTER(Integer), update_spiral([http, received, get], Integer)).

%% ======================================================
%% API
%% ======================================================

start(_StartType, _StartArgs) ->
    application:ensure_all_started(exometer_core),
    {ok, ListenSocket} = gen_tcp:listen(1717, [{reuseaddr, true}, binary]),
    spawn(simple_stats_app, receiver, [ListenSocket]),
    simple_stats_sup:start_link().

stop(_State) ->
    ok.

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

receiver(ListenSocket) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    ok = inet:setopts(AcceptSocket, [{packet, http_bin}]),

    receive
        {http, _, _Msg} ->
            ?INCREMENT_HTTP_RECEIVED_GET_COUNTER(1),
            BinaryResult = maps:fold(
                fun(K,V, Acc) ->
                    BinaryKey = convert_key(K),
                    BinaryValues = convert_value(V, BinaryKey, <<>>),
                    <<Acc/binary, BinaryValues/binary >>
                end, <<>>, stats:get_stats()),

            ok = inet:setopts(AcceptSocket, [{packet, raw}]),
            ok = gen_tcp:send(AcceptSocket, reply(BinaryResult)),
            gen_tcp:close(AcceptSocket);
        _Other ->
            ok
    end,
    receiver(ListenSocket).

convert_key(Key) when is_atom(Key) ->
    atom_to_binary(Key, latin1);
convert_key(Key) when is_integer(Key) ->
    integer_to_binary(Key);
convert_key(Key) when is_tuple(Key) ->
    List = tuple_to_list(Key),
    lists:foldl(fun(V, Acc) -> << Acc/binary, (atom_to_binary(V, latin1))/binary , ".">> end, <<>>, List).

convert_value([], _, Acc) ->
    Acc;
convert_value([{Key,Value} | Rest], BinaryKey, Acc) ->
    BinaryKey2 = convert_key(Key),
    BinaryValue = convert_key(Value),
    convert_value(Rest, BinaryKey, << Acc/binary, BinaryKey/binary, BinaryKey2/binary, "=", BinaryValue/binary, "\n" >>).

reply(Result) ->
    <<"HTTP/1.0 200 OK\r\n",
    "Content-Length: ", (integer_to_binary(byte_size(Result)))/binary, "\r\n\r\n", Result/binary,
    "\n">>.