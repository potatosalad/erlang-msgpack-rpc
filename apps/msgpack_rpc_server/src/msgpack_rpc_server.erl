%%% @author UENISHI Kota <kota@basho.com>
%%% @copyright (C) 2012, UENISHI Kota
%%% @doc
%%%
%%% @end
%%% Created : 29 Sep 2012 by UENISHI Kota <kota@basho.com>

-module(msgpack_rpc_server).

%% API.
-export([start_tcp/4,
         start_ssl/4,
         stop_listener/1]).

%% Stateless API.
-export([start/4,
         start/5,
         stop/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Start a TCP listener.
% -spec start_tcp(ranch:ref(), non_neg_integer(), ranch_tcp:opts(),
%     msgpack_rpc_protocol:opts(), module(), any()) -> {ok, pid()}.
start_tcp(Ref, NbAcceptors, TransOpts, ProtoOpts)
        when is_integer(NbAcceptors), NbAcceptors > 0 ->
    ranch:start_listener(Ref, NbAcceptors,
        ranch_tcp, TransOpts, msgpack_rpc_protocol, ProtoOpts).

%% @doc Start a SSL listener.
% -spec start_ssl(ranch:ref(), non_neg_integer(), ranch_ssl:opts(),
%     msgpack_rpc_protocol:opts(), module(), any()) -> {ok, pid()}.
start_ssl(Ref, NbAcceptors, TransOpts, ProtoOpts)
        when is_integer(NbAcceptors), NbAcceptors > 0 ->
    ranch:start_listener(Ref, NbAcceptors,
        ranch_ssl, TransOpts, msgpack_rpc_protocol, ProtoOpts).

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
start(Name, NumProc, ssl, Service, Opts) ->
    % msgpack_rpc_server_sup:start_link(),
    msgpack_rpc:start_ssl(Name, NumProc, Opts, [{env, [{handler, msgpack_rpc_stateless}, {handler_opts, [{service, Service}]}]}]);
start(Name, NumProc, tcp, Service, Opts) ->
    % msgpack_rpc_server_sup:start_link(),
    msgpack_rpc:start_tcp(Name, NumProc, Opts, [{env, [{handler, msgpack_rpc_stateless}, {handler_opts, [{service, Service}]}]}]).

-spec stop(ranch:ref()) -> ok.
stop(Name) ->
    stop_listener(Name).
