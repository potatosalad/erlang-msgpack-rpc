#!/usr/bin/env escript
%%! -smp enable -pa ebin -env ERL_LIBS apps:deps -input
-module(simple_service).
-mode(compile).

-export([main/1]).

%% msgpack_rpc methods
-export([echo/1,
         sum/2]).

main(_) ->
    Port = 18800,
    ok = application:start(crypto),
    ok = application:start(ranch),

    io:format(" [*] Running at localhost:~p~n", [Port]),

    msgpack_rpc_server:start_service(msgpack_rpc_simple_service, 4, ranch_tcp, simple_service, [{port, Port}]),
    % shell:start(),

    receive
        _ -> ok
    end.

%%====================================================================
%% msgpack_rpc methods
%%====================================================================

echo(Message) ->
    io:format("[echo] ~p~n", [Message]),
    Message.

sum(X, Y) ->
    Sum = X + Y,
    io:format("[sum] ~p + ~p = ~p~n", [X, Y, Sum]),
    Sum.
