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
    Transport2 = msgpack_rpc:known_transport(Transport),
    ranch:start_listener(Ref, NbAcceptors, Transport2, TransOpts, msgpack_rpc_protocol, ProtoOpts).

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

-spec start(ranch:ref(), non_neg_integer(), tcp | ssl | ranch_tcp | ranch_ssl, module(), any())
    -> {ok, pid()}.
start(Name, NumProc, Transport, Service, Opts) ->
    start_listener(Name, NumProc, Transport, Opts,
        [{handler, msgpack_rpc_stateless},
         {handler_opts,
            [{service, Service}]}]).

-spec stop(ranch:ref()) -> ok.
stop(Name) ->
    stop_listener(Name).

%% Tests.

-ifdef(TEST).

start_stop_test() ->
    application:start(ranch),
    application:start(crypto),
    application:start(msgpack_rpc_server),
    {ok, _} = msgpack_rpc_server:start(test_msgpack_rpc_server, 3, tcp, dummy, [{port, 9199}]),
    ok = msgpack_rpc_server:stop(test_msgpack_rpc_server),
    application:stop(msgpack_rpc_server),
    application:stop(crypto),
    application:stop(ranch).

-endif.
