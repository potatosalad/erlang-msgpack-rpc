#!/usr/bin/env escript
%%! -smp enable -pa ebin -env ERL_LIBS deps -input
-module(test_service).
-mode(compile).

-export([main/1]).

-export([msgpack_rpc_init/2]).
-export([msgpack_rpc_request/2]).
-export([msgpack_rpc_notify/2]).
-export([msgpack_rpc_info/2]).
-export([msgpack_rpc_terminate/2]).

-export([sum/2]).

main(_) ->
    Port = 8081,
    ok = application:start(ranch),
    ok = application:start(crypto),

    io:format(" [*] Running at http://localhost:~p~n", [Port]),

    msgpack_rpc:start_tcp(test_service, 100, [{port, Port}], [], msgpack_rpc_service, [{service, test_service}]),

    receive
        _ -> ok
    end.

msgpack_rpc_init(Name, Opts) ->
    io:format("[init] Name: ~p Opts: ~p~n", [Name, Opts]),
    {ok, undefined}.

msgpack_rpc_request(Req, State) ->
    io:format("[request] Req: ~p State: ~p~n", [Req, State]),
    {Method, Req} = msgpack_rpc_request:method(Req),
    {Params, Req} = msgpack_rpc_request:params(Req),
    Function = binary_to_existing_atom(Method, latin1),
    apply(?MODULE, Function, Params),
    {ok, State}.

msgpack_rpc_notify(Req, State) ->
    io:format("[notify] Req: ~p State: ~p~n", [Req, State]),
    {ok, State}.

msgpack_rpc_info(Req, State) ->
    io:format("[info] Req: ~p State: ~p~n", [Req, State]),
    {ok, State}.

msgpack_rpc_terminate(Reason, State) ->
    io:format("[terminate] Reason: ~p State: ~p~n", [Reason, State]),
    ok.

sum(X, Y) ->
    io:format("~p + ~p = ~p~n", [X, Y, X + Y]),
    X + Y.
