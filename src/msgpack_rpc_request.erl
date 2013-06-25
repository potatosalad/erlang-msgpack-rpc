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

%% API
-export([new/3, get/2, to_msgpack_object/1]).

-type obj() :: #msgpack_rpc_request{}.
-export_type([obj/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(msgpack_rpc:msg_id(), msgpack_rpc:method(), msgpack_rpc:params()) -> obj().
new(MsgId, Method, Params) ->
    #msgpack_rpc_request{msg_id=MsgId, method=Method, params=Params}.

-spec get(list(atom()) | atom(), obj()) -> list(term()) | term().
get(List, Obj) when is_list(List) ->
    [g(Atom, Obj) || Atom <- List];
get(Atom, Obj) when is_atom(Atom) ->
    g(Atom, Obj).

-spec to_msgpack_object(obj()) -> msgpack:object().
to_msgpack_object(Obj) ->
    [?MSGPACK_RPC_REQUEST | get([msg_id, method, params], Obj)].

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
g(msg_id, #msgpack_rpc_request{msg_id=Ret}) -> Ret;
g(method, #msgpack_rpc_request{method=Ret}) -> Ret;
g(params, #msgpack_rpc_request{params=Ret}) -> Ret.
