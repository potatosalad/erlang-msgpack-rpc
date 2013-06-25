%%% @author UENISHI Kota <kota@basho.com>
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright (C) 2012, UENISHI Kota
%%%                2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 29 Sep 2012 by UENISHI Kota <kota@basho.com>

-module(msgpack_rpc_server).

%% API.
-export([start_listener/5,
         start_service/5,
         stop_listener/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec start_listener(ranch:ref(), non_neg_integer(), module(), any(), any())
    -> {ok, pid()}.
start_listener(Ref, NbAcceptors, Transport, TransOpts, ProtoOpts)
        when is_integer(NbAcceptors), NbAcceptors > 0 ->
    Transport2 = msgpack_rpc_util:known_transport(Transport),
    ranch:start_listener(Ref, NbAcceptors, Transport2, TransOpts, msgpack_rpc_protocol, ProtoOpts).

-spec start_service(ranch:ref(), non_neg_integer(), module(), module(), any())
    -> {ok, pid()}.
start_service(Name, NumProc, Transport, Service, Opts) ->
    start_listener(Name, NumProc, Transport, Opts,
        [{handler, msgpack_rpc_stateless},
         {handler_opts,
            [{service, Service}]}]).

%% @doc Stop a listener.
-spec stop_listener(ranch:ref()) -> ok.
stop_listener(Ref) ->
    ranch:stop_listener(Ref).

%% Tests.

-ifdef(TEST).

start_service_test() ->
    application:start(ranch),
    application:start(crypto),
    application:start(msgpack_rpc_server),
    {ok, _} = msgpack_rpc_server:start_service(test_msgpack_rpc_server, 3, ranch_tcp, dummy, [{port, 9199}]),
    ok = msgpack_rpc_server:stop_listener(test_msgpack_rpc_server),
    application:stop(msgpack_rpc_server),
    application:stop(crypto),
    application:stop(ranch).

-endif.
