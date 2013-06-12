%%% @author UENISHI Kota <kota@basho.com>
%%% @copyright (C) 2012, UENISHI Kota
%%% @doc
%%%
%%% @end
%%% Created : 29 Sep 2012 by UENISHI Kota <kota@basho.com>

-module(msgpack_rpc_server).

%% API.
-export([start_listener/5,
         stop_listener/1]).

%% Stateless API.
-export([start/4,
         start/5,
         stop/1]).

%%====================================================================
%% API functions
%%====================================================================

start_listener(Ref, NbAcceptors, Transport, TransOpts, ProtoOpts)
        when is_integer(NbAcceptors), NbAcceptors > 0 ->
    ranch:start_listener(Ref, NbAcceptors, Transport, TransOpts, msgpack_rpc_protocol, ProtoOpts).

%% @doc Stop a listener.
-spec stop_listener(ranch:ref()) -> ok.
stop_listener(Ref) ->
    ranch:stop_listener(Ref).

%%====================================================================
%% Stateless API functions
%%====================================================================

-spec start(ranch:ref(), tcp | ssl, module(), any())
    -> {ok, pid()} | {error, term()}.
start(Name, Transport, Service, Opts) ->
    start(Name, 4, Transport, Service, Opts).

-spec start(ranch:ref(), non_neg_integer(), tcp | ssl, module(), any())
    -> {ok, pid()}.
start(Name, NumProc, Transport, Service, Opts) ->
    Transport2 = msgpack_rpc:transport(Transport),
    start_listener(Name, NumProc, Transport2, Opts,
        [{handler, msgpack_rpc_stateless},
         {handler_opts,
            [{service, Service}]}]).

-spec stop(ranch:ref()) -> ok.
stop(Name) ->
    stop_listener(Name).

%% Tests.

-ifdef(TEST).

start_stop_test() ->
    ok = application:start(ranch),
    ok = application:start(crypto),
    ok = application:start(msgpack_rpc_server),
    {ok, _} = msgpack_rpc_server:start(test_msgpack_rpc_server, 3, tcp, dummy, [{port, 9199}]),
    ok = msgpack_rpc_server:stop(test_msgpack_rpc_server),
    ok = application:stop(msgpack_rpc_server),
    ok = application:stop(crypto),
    ok = application:stop(ranch).

-endif.
