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
-export([new/4, get/2, to_msgpack_object/1]).

-type obj() :: #msgpack_rpc_response{}.
-export_type([obj/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(msgpack_rpc:request(), msgpack_rpc:msg_id(), msgpack_rpc:error(), msgpack_rpc:result()) -> obj().
new(Request, MsgId, Error, Result) ->
    #msgpack_rpc_response{request=Request, msg_id=MsgId, error=Error, result=Result}.

-spec get(list(atom()) | atom(), obj()) -> list(term()) | term().
get(List, Obj) when is_list(List) ->
    [g(Atom, Obj) || Atom <- List];
get(Atom, Obj) when is_atom(Atom) ->
    g(Atom, Obj).

-spec to_msgpack_object(obj()) -> msgpack:object().
to_msgpack_object(Obj) ->
    [?MSGPACK_RPC_RESPONSE | get([msg_id, error, result], Obj)].

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
g(request, #msgpack_rpc_response{request=Ret}) -> Ret;
g(msg_id, #msgpack_rpc_response{msg_id=Ret}) -> Ret;
g(error, #msgpack_rpc_response{error=Ret}) -> Ret;
g(result, #msgpack_rpc_response{result=Ret}) -> Ret.
