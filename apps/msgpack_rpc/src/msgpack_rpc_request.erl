%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  10 June 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(msgpack_rpc_request).

-include("msgpack_rpc.hrl").
-include("msgpack_rpc_protocol.hrl").

%% API
-export([new/3, msg_id/1, method/1, params/1, to_msgpack_object/1]).

%% private API
-export([get/2]).

-type obj() :: #msgpack_rpc_request{}.
-export_type([obj/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(msgpack_rpc:msg_id(), msgpack_rpc:method(), msgpack_rpc:params())
    -> obj().
new(MsgId, Method, Params) ->
    #msgpack_rpc_request{msg_id=MsgId, method=Method, params=Params}.

%% @doc Return the msgpack_rpc msg_id of the request.
-spec msg_id(Obj) -> {msgpack_rpc:msg_id(), Obj} when Obj::obj().
msg_id(Obj) ->
    {Obj#msgpack_rpc_request.msg_id, Obj}.

%% @doc Return the msgpack_rpc method of the request.
-spec method(Obj) -> {msgpack_rpc:method(), Obj} when Obj::obj().
method(Obj) ->
    {Obj#msgpack_rpc_request.method, Obj}.

%% @doc Return the msgpack_rpc params of the request.
-spec params(Obj) -> {msgpack_rpc:params(), Obj} when Obj::obj().
params(Obj) ->
    {Obj#msgpack_rpc_request.params, Obj}.

% -spec to_msgpack(msgpack_rpc:request())
%     -> [0, msgpack_rpc:msg_id(), msgpack_rpc:method(), msgpack_rpc:params()].
to_msgpack_object(Obj) ->
    [?MSGPACK_RPC_REQUEST | get([msg_id, method, params], Obj)].

%% @private
get(List, Obj) when is_list(List) ->
    [g(Atom, Obj) || Atom <- List];
get(Atom, Obj) when is_atom(Atom) ->
    g(Atom, Obj).

g(msg_id, #msgpack_rpc_request{msg_id=Ret}) -> Ret;
g(method, #msgpack_rpc_request{method=Ret}) -> Ret;
g(params, #msgpack_rpc_request{params=Ret}) -> Ret.
