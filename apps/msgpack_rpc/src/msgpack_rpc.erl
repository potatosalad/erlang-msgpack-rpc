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

-include("msgpack_rpc.hrl").
-include("msgpack_rpc_protocol.hrl").

%% API
-export([start_client/4,
         start_listener/5,
         stop_client/1,
         stop_listener/1]).

%% Utility API
-export([binary_to_known_error/1,
         known_error_to_binary/1,
         method_to_binary/1,
         partition_options/2,
         transport/1]).

-type msg_id() :: non_neg_integer().
-type method() :: binary().
-type params() :: msgpack:object().
-type error()  :: msgpack:object().
-type result() :: msgpack:object().

-export_type([msg_id/0,
              method/0,
              params/0,
              error/0,
              result/0]).

-type request()  :: #msgpack_rpc_request{}.
-type response() :: #msgpack_rpc_response{}.
-type notify()   :: #msgpack_rpc_notify{}.
-type options()  :: msgpack_rpc_options:obj().

-export_type([request/0,
              response/0,
              notify/0,
              options/0]).

%% API

start_client(Transport, Address, Port, Opts) ->
    msgpack_rpc_client:start_link(Transport, Address, Port, Opts).

start_listener(Ref, NbAcceptors, Transport, TransOpts, ProtoOpts) ->
    msgpack_rpc_server:start_listener(Ref, NbAcceptors, Transport, TransOpts, ProtoOpts).

%% @doc Stop a client.
-spec stop_client(msgpack_rpc_client:ref()) -> ok.
stop_client(Ref) ->
    msgpack_rpc_client:stop(Ref).

%% @doc Stop a listener.
-spec stop_listener(ranch:ref()) -> ok.
stop_listener(Ref) ->
    msgpack_rpc_server:stop_listener(Ref).

%% Utility API

binary_to_known_error(nil) -> nil;
binary_to_known_error(<<"undef">>) -> undef;
binary_to_known_error(<<"function_clause">>) -> function_clause;
binary_to_known_error(<<"unknown_error">>) -> unknown_error;
binary_to_known_error(Error) -> Error.

known_error_to_binary(nil) -> nil;
known_error_to_binary(undef) -> <<"undef">>;
known_error_to_binary(function_clause) -> <<"function_clause">>;
known_error_to_binary(_) -> <<"unknown_error">>.

method_to_binary(Method) when is_atom(Method) ->
    atom_to_binary(Method, latin1);
method_to_binary(Method) when is_list(Method) ->
    iolist_to_binary(Method);
method_to_binary(Method) ->
    Method.

partition_options(Opts, Keys) ->
    {Satisfying, NotSatisfying} = proplists:split(Opts, Keys),
    {lists:flatten(Satisfying), NotSatisfying}.

transport(ssl) ->
    ranch_ssl;
transport(tcp) ->
    ranch_tcp;
transport(Transport) ->
    Transport.
