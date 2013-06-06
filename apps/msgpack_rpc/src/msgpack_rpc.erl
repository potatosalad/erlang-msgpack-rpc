%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  31 May 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------

%% @doc Convenience API to start and stop TCP/SSL listeners.
-module(msgpack_rpc).

-export([start_tcp/4]).
-export([start_ssl/4]).
-export([stop_listener/1]).

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
