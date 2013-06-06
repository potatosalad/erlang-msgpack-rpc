%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  31 May 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------

-module(msgpack_rpc_notify).

-include("msgpack_rpc_server.hrl").

%% Notify API.
-export([start_fsm/2]).

-export([new/2]).

-export([method/1]).
-export([params/1]).

-export([get/2]).
-export([set/2]).

-export([execute/2]).
-export([finalize/1]).

-opaque req() :: #msgpack_rpc_notify{}.
-export_type([req/0]).

start_fsm(Method, Params) ->
    msgpack_rpc_notify_fsm:start(Method, Params).

-spec new(binary(), any()) -> req().
new(Method, Params) ->
    #msgpack_rpc_notify{method=Method, params=Params}.

%% @doc Return the msgpack_rpc method of the notify.
-spec method(Req) -> {binary(), Req} when Req::req().
method(Req) ->
    {Req#msgpack_rpc_notify.method, Req}.

%% @doc Return the msgpack_rpc params of the notify.
-spec params(Req) -> {binary(), Req} when Req::req().
params(Req) ->
    {Req#msgpack_rpc_notify.params, Req}.

get(List, Req) when is_list(List) ->
    [g(Atom, Req) || Atom <- List];
get(Atom, Req) when is_atom(Atom) ->
    g(Atom, Req).

g(method, #msgpack_rpc_notify{method=Ret}) -> Ret;
g(params, #msgpack_rpc_notify{params=Ret}) -> Ret.

set([], Req) -> Req;
set([{fsm_ref, Val} | Tail], Req) -> set(Tail, Req#msgpack_rpc_notify{fsm_ref=Val}).

execute(_, Req=#msgpack_rpc_notify{executed=true}) ->
    {ok, Req};
execute(Task, Req=#msgpack_rpc_notify{fsm_ref=FsmRef}) ->
    {msgpack_rpc_notify_fsm:execute(Task, FsmRef), Req#msgpack_rpc_notify{executed=true}}.

finalize(Req=#msgpack_rpc_notify{fsm_ref=FsmRef}) ->
    {msgpack_rpc_notify_fsm:finalize(FsmRef), Req}.
