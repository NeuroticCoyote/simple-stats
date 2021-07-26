%%%-------------------------------------------------------------------
%%% @author joegoodwin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2021 14:32
%%%-------------------------------------------------------------------
-module(http_handler).
-author("joegoodwin").

%% API
-export([start_http_handler/0, receiver/1]).

-define(INCREMENT_HTTP_RECEIVED_GET_COUNTER(), simple_stats:update_spiral([http, received, get], 1)).

start_http_handler() ->
	{ok, ListenSocket} = gen_tcp:listen(1717, [{reuseaddr, true}, binary]),
	spawn(http_handler, receiver, [ListenSocket]).

%% used to render the HTML page, since we dont want to use a lib like cowboy
receiver(ListenSocket) ->
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	ok = inet:setopts(AcceptSocket, [{packet, http_bin}]),

	receive
		{http, _, _Msg} ->
			?INCREMENT_HTTP_RECEIVED_GET_COUNTER(),
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