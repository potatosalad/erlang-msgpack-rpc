%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  10 June 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(msgpack_rpc_notify).

-include("msgpack_rpc.hrl").

%% API
-export([new/2, get/2, to_msgpack_object/1]).

-type obj() :: #msgpack_rpc_notify{}.
-export_type([obj/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(msgpack_rpc:method(), msgpack_rpc:params()) -> obj().
new(Method, Params) ->
    #msgpack_rpc_notify{method=Method, params=Params}.

-spec get(list(atom()) | atom(), obj()) -> list(term()) | term().
get(List, Obj) when is_list(List) ->
    [g(Atom, Obj) || Atom <- List];
get(Atom, Obj) when is_atom(Atom) ->
    g(Atom, Obj).

-spec to_msgpack_object(obj()) -> {ok, msgpack:object()}.
to_msgpack_object(Obj) ->
    [?MSGPACK_RPC_NOTIFY | get([method, params], Obj)].

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
g(method, #msgpack_rpc_notify{method=Ret}) -> Ret;
g(params, #msgpack_rpc_notify{params=Ret}) -> Ret.
