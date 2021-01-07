#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname factorial -mnesia debug verbose
main([]) ->
    Name = list_to_atom("simple_stats@Joes-MacBook-Pro"),
    erlang:set_cookie(Name, cookie),
    case net_adm:ping(Name) of
        pong ->
            Result = rpc:call(Name, stats, get_stats, []),
            io:format("~p~n", [Result]);
        _Other ->
             io:format("Could not ping ~p~n", [Name])
    end;
main(_) ->
    halt(1).
