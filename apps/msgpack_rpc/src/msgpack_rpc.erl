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

%% API
-export([connect/3,
         start_listener/5,
         stop_client/1,
         stop_listener/1]).

%% Helpers
-export([binary_to_known_error/1,
         known_error_to_binary/1,
         known_transport/1,
         method_to_binary/1,
         partition_options/2]).

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
-type options()  :: #msgpack_rpc_options{}.

-export_type([request/0,
              response/0,
              notify/0,
              options/0]).

-type client() :: msgpack_rpc_client:obj().

-export_type([client/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec connect(inet:ip_address() | inet:hostname(), inet:port_number(), [proplists:property()])
    -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
connect(Address, Port, Opts) ->
    msgpack_rpc_client:connect(Address, Port, Opts).

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

%%%===================================================================
%%% Helpers
%%%===================================================================

binary_to_known_error(nil) -> nil;
binary_to_known_error(<<"undef">>) -> undef;
binary_to_known_error(<<"function_clause">>) -> function_clause;
binary_to_known_error(<<"unknown">>) -> unknown;
binary_to_known_error(Error) -> Error.

known_error_to_binary(nil) -> nil;
known_error_to_binary(undef) -> <<"undef">>;
known_error_to_binary(function_clause) -> <<"function_clause">>;
known_error_to_binary(_) -> <<"unknown">>.

-spec known_transport(atom()) -> atom().
known_transport(ssl) ->
    ranch_ssl;
known_transport(tcp) ->
    ranch_tcp;
known_transport(Transport) when is_atom(Transport) ->
    Transport.

-spec method_to_binary(atom() | list() | msgpack:object()) -> msgpack:object().
method_to_binary(Method) when is_atom(Method) ->
    atom_to_binary(Method, latin1);
method_to_binary(Method) when is_list(Method) ->
    iolist_to_binary(Method);
method_to_binary(Method) ->
    Method.

-spec partition_options([proplists:property()], [atom()]) -> {[proplists:property()], [proplists:property()]}.
partition_options(Opts, Keys) ->
    {Satisfying, NotSatisfying} = proplists:split(Opts, Keys),
    {lists:flatten(Satisfying), NotSatisfying}.

-ifdef(TEST).

binary_to_known_error_test() ->
    nil = binary_to_known_error(nil),
    undef = binary_to_known_error(<<"undef">>),
    function_clause = binary_to_known_error(<<"function_clause">>),
    unknown = binary_to_known_error(<<"unknown">>),
    {} = binary_to_known_error({}).

known_error_to_binary_test() ->
    nil = known_error_to_binary(nil),
    <<"undef">> = known_error_to_binary(undef),
    <<"function_clause">> = known_error_to_binary(function_clause),
    <<"unknown">> = known_error_to_binary({}).

known_transport_test() ->
    ranch_ssl = known_transport(ssl),
    ranch_ssl = known_transport(ranch_ssl),
    ranch_tcp = known_transport(tcp),
    ranch_tcp = known_transport(ranch_tcp),
    other     = known_transport(other).

method_to_binary_test() ->
    <<"atom">> = method_to_binary(atom),
    <<"list">> = method_to_binary("list"),
    <<"term">> = method_to_binary(<<"term">>).

partition_options_test() ->
    Tests = [
        {[], [], {[],[]}},
        {[a,{b,2},c,{d,4}], [b,d], {[{b,2},{d,4}], [a,c]}},
        {[{a,1},{a,2},{b,3},{c,4}], [a], {[{a,1},{a,2}], [{b,3},{c,4}]}}
    ],
    _ = [Ret = begin
        partition_options(Opts, Keys)
    end || {Opts, Keys, Ret} <- Tests],
    ok.

-endif.
