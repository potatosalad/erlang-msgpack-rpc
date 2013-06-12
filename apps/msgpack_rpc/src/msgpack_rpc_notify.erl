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
-include("msgpack_rpc_protocol.hrl").

%% API
-export([new/2, method/1, params/1, to_msgpack_object/1]).

%% private API
-export([get/2]).

-type req() :: #msgpack_rpc_notify{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec new(msgpack_rpc:method(), msgpack_rpc:params()) -> req().
new(Method, Params) ->
    #msgpack_rpc_notify{method=Method, params=Params}.

%% @doc Return the msgpack_rpc method of the request.
-spec method(Req) -> {msgpack_rpc:method(), Req} when Req::req().
method(Req) ->
    {Req#msgpack_rpc_notify.method, Req}.

%% @doc Return the msgpack_rpc params of the request.
-spec params(Req) -> {msgpack_rpc:params(), Req} when Req::req().
params(Req) ->
    {Req#msgpack_rpc_notify.params, Req}.

% -spec to_msgpack(msgpack_rpc:notify())
%     -> [2, msgpack_rpc:method(), msgpack_rpc:params()].
to_msgpack_object(Req) ->
    [?MSGPACK_RPC_NOTIFY | get([method, params], Req)].

%% @private
get(List, Req) when is_list(List) ->
    [g(Atom, Req) || Atom <- List];
get(Atom, Req) when is_atom(Atom) ->
    g(Atom, Req).

g(method, #msgpack_rpc_notify{method=Ret}) -> Ret;
g(params, #msgpack_rpc_notify{params=Ret}) -> Ret.
