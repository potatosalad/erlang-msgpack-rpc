%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  10 June 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(msgpack_rpc_response).

-include("msgpack_rpc.hrl").

%% API
-export([new/4, request/1, msg_id/1, error/1, result/1, to_msgpack_object/1]).

%% private API
-export([get/2]).

-type req() :: #msgpack_rpc_response{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec new(msgpack_rpc:request(), msgpack_rpc:msg_id(), msgpack_rpc:error(), msgpack_rpc:result())
    -> req().
new(Request, MsgId, Error, Result) ->
    #msgpack_rpc_response{request=Request, msg_id=MsgId, error=Error, result=Result}.

%% @doc Return the msgpack_rpc request of the response.
-spec request(Req) -> {msgpack_rpc:request(), msgpack_rpc:response()} when Req::req().
request(Req) ->
    {Req#msgpack_rpc_response.request, Req}.

%% @doc Return the msgpack_rpc msg_id of the response.
-spec msg_id(Req) -> {msgpack_rpc:msg_id(), Req} when Req::req().
msg_id(Req) ->
    {Req#msgpack_rpc_response.msg_id, Req}.

%% @doc Return the msgpack_rpc error of the response.
-spec error(Req) -> {msgpack_rpc:error(), Req} when Req::req().
error(Req) ->
    {Req#msgpack_rpc_response.error, Req}.

%% @doc Return the msgpack_rpc result of the response.
-spec result(Req) -> {msgpack_rpc:result(), Req} when Req::req().
result(Req) ->
    {Req#msgpack_rpc_response.result, Req}.

% -spec to_msgpack(msgpack_rpc:response())
%     -> [1, msgpack_rpc:msg_id(), msgpack_rpc:error(), msgpack_rpc:result()].
to_msgpack_object(Req) ->
    [?MSGPACK_RPC_RESPONSE | get([msg_id, error, result], Req)].

%% @private
get(List, Req) when is_list(List) ->
    [g(Atom, Req) || Atom <- List];
get(Atom, Req) when is_atom(Atom) ->
    g(Atom, Req).

g(request, #msgpack_rpc_response{request=Ret}) -> Ret;
g(msg_id, #msgpack_rpc_response{msg_id=Ret}) -> Ret;
g(error, #msgpack_rpc_response{error=Ret}) -> Ret;
g(result, #msgpack_rpc_response{result=Ret}) -> Ret.
