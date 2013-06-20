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

%% Client API
-export([connect/3,
         close/1]).

%% Server API
-export([start_listener/5,
         stop_listener/1]).

-type msg_id() :: non_neg_integer().
-type method() :: binary() | atom() | list().
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
-type task()   :: msgpack_rpc_task:obj().

-export_type([client/0,
              task/0]).

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
