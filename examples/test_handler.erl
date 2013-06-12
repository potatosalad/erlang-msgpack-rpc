#!/usr/bin/env escript
%%! -smp enable -pa ebin -env ERL_LIBS apps:deps -input
-module(test_handler).
-mode(compile).
-behaviour(msgpack_rpc_handler).

-export([main/1]).

%% msgpack_rpc_handler callbacks
-export([msgpack_rpc_init/2,
         msgpack_rpc_handle/3,
         msgpack_rpc_info/2,
         msgpack_rpc_terminate/3]).

-export([binary_to_error/1,
         error_to_binary/1]).

main(_) ->
    Port = 8081,
    ok = application:start(crypto),
    ok = application:start(ranch),

    io:format(" [*] Running at localhost:~p~n", [Port]),

    msgpack_rpc:start_listener(test_handler, 100, ranch_tcp, [{port, Port}],
        [{handler, test_handler},
         {error_decoder, fun test_handler:binary_to_error/1},
         {error_encoder, fun test_handler:error_to_binary/1},
         {msgpack_packer, fun msgpack_nif:pack/1},
         {msgpack_unpacker, fun msgpack_nif:unpack_stream/1}]),
    shell:start(),

    receive
        _ -> ok
    end.

msgpack_rpc_init(Type, Opts) ->
    io:format("[init] Type: ~p Opts: ~p~n", [Type, Opts]),
    {ok, undefined}.

msgpack_rpc_handle({Type, Method, Params}, Task, State) ->
    io:format("[~p] Task: ~p State: ~p~n", [Type, Task, State]),
    Job = case Method of
        <<"sum">> ->
            fun() ->
                [X, Y] = Params,
                {respond, {result, X + Y}}
            end;
        <<"echo">> ->
            fun() ->
                {respond, {result, Params}}
            end;
        <<"test">> ->
            fun() ->
                try
                    case Method of
                        ok -> ok
                    end
                catch
                    Class:Reason ->
                        {respond, {error, {Reason, erlang:get_stacktrace()}}}
                end
            end;
        _ ->
            fun() ->
                erlang:error(undef)
            end
    end,
    {execute, Job, Task, State, hibernate}.

% msgpack_rpc_notify(Task, State) ->
%     io:format("[notify] Task: ~p State: ~p~n", [Task, State]),
%     {ok, Task, State}.

msgpack_rpc_info(Info, State) ->
    io:format("[info] Info: ~p State: ~p~n", [Info, State]),
    {ok, State}.

msgpack_rpc_terminate(Reason, Task, State) ->
    io:format("[terminate] Reason: ~p Task: ~p State: ~p~n", [Reason, Task, State]),
    ok.

binary_to_error(nil) -> nil;
binary_to_error(Binary) ->
    try
        binary_to_existing_atom(Binary, latin1)
    catch
        _:_ ->
            try
                binary_to_term(Binary)
            catch
                _:_ ->
                    Binary
            end
    end.

error_to_binary(nil) -> nil;
error_to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, latin1);
error_to_binary(Error) ->
    case msgpack:pack(Error) of
        {error, _} ->
            term_to_binary(Error);
        _ ->
            Error
    end.
