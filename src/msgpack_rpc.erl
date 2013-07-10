%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc Convenience API to start and stop TCP/SSL clients and listeners.
%%%
%%% @end
%%% Created :  31 May 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(msgpack_rpc).

-include("msgpack_rpc.hrl").

%% Client API
-export([connect/3,
         close/1]).

%% Server API
-export([start_listener/5,
         stop_listener/1,
         reply/2]).

-type msg_id() :: non_neg_integer().
-type method() :: binary() | atom() | list().
-type params() :: msgpack:object().
-type error()  :: nil | msgpack:object().
-type result() :: nil | msgpack:object().

-export_type([msg_id/0,
              method/0,
              params/0,
              error/0,
              result/0]).

-type request()  :: msgpack_rpc_request:obj().
-type response() :: msgpack_rpc_response:obj().
-type notify()   :: msgpack_rpc_notify:obj().
-type options()  :: msgpack_rpc_options:obj().

-export_type([request/0,
              response/0,
              notify/0,
              options/0]).

-type client() :: msgpack_rpc_client:obj().
-type reply()  :: {error, error()} | {result, result()}.

-export_type([client/0,
              reply/0]).

%%%===================================================================
%%% Client API functions
%%%===================================================================

%% @doc Connect a client.
-spec connect(inet:ip_address() | inet:hostname(), inet:port_number(), [proplists:property()])
    -> {ok, client()} | ignore | {error, {already_started, pid()} | term()}.
connect(Address, Port, Opts) ->
    msgpack_rpc_client:connect(Address, Port, Opts).

%% @doc Close a client.
-spec close(client()) -> ok.
close(Client) ->
    msgpack_rpc_client:close(Client).

%%%===================================================================
%%% Server API functions
%%%===================================================================

%% @doc Start a listener.
-spec start_listener(ranch:ref(), non_neg_integer(),
        tcp | ssl | ranch_tcp | ranch_ssl,
        [proplists:property()], [proplists:property()])
    -> {ok, pid()} | {error, badarg}.
start_listener(Ref, NbAcceptors, Transport, TransOpts, ProtoOpts) ->
    msgpack_rpc_server:start_listener(Ref, NbAcceptors, Transport, TransOpts, ProtoOpts).

%% @doc Stop a listener.
-spec stop_listener(ranch:ref()) -> ok.
stop_listener(Ref) ->
    msgpack_rpc_server:stop_listener(Ref).

reply({To, MsgId}, {error, Error}) ->
    catch To ! {'$msgpack_rpc_response', MsgId, Error, nil};
reply({To, MsgId}, {result, Result}) ->
    catch To ! {'$msgpack_rpc_response', MsgId, nil, Result}.
