%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  31 May 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------

-module(msgpack_rpc_request).

-include("msgpack_rpc_server.hrl").

%% Request API.
-export([start_fsm/6]).

-export([new/6]).

-export([msg_id/1]).
-export([method/1]).
-export([params/1]).

-export([error/2]).
-export([result/2]).
-export([response/3]).

-export([get/2]).
-export([set/2]).

-export([execute/2]).
-export([finalize/1]).
-export([reply/3]).

-opaque req() :: #msgpack_rpc_request{}.
-export_type([req/0]).

start_fsm(MsgId, Method, Params, Socket, Transport, MsgpackOpts) ->
    msgpack_rpc_request_fsm:start(MsgId, Method, Params, Socket, Transport, MsgpackOpts).

-spec new(non_neg_integer(), binary(), any(), inet:socket(), module(), any()) -> req().
new(MsgId, Method, Params, Socket, Transport, MsgpackOpts) ->
    #msgpack_rpc_request{msg_id=MsgId, method=Method, params=Params,
        socket=Socket, transport=Transport, msgpack_opts=MsgpackOpts}.

%% @doc Return the msgpack_rpc msg_id of the request.
-spec msg_id(Req) -> {binary(), Req} when Req::req().
msg_id(Req) ->
    {Req#msgpack_rpc_request.msg_id, Req}.

%% @doc Return the msgpack_rpc method of the request.
-spec method(Req) -> {binary(), Req} when Req::req().
method(Req) ->
    {Req#msgpack_rpc_request.method, Req}.

%% @doc Return the msgpack_rpc params of the request.
-spec params(Req) -> {binary(), Req} when Req::req().
params(Req) ->
    {Req#msgpack_rpc_request.params, Req}.

get(List, Req) when is_list(List) ->
    [g(Atom, Req) || Atom <- List];
get(Atom, Req) when is_atom(Atom) ->
    g(Atom, Req).

g(msg_id, #msgpack_rpc_request{msg_id=Ret}) -> Ret;
g(method, #msgpack_rpc_request{method=Ret}) -> Ret;
g(params, #msgpack_rpc_request{params=Ret}) -> Ret;
g(socket, #msgpack_rpc_request{socket=Ret}) -> Ret;
g(transport, #msgpack_rpc_request{transport=Ret}) -> Ret.

set([], Req) -> Req;
set([{fsm_ref, Val} | Tail], Req) -> set(Tail, Req#msgpack_rpc_request{fsm_ref=Val}).

execute(_, Req=#msgpack_rpc_request{executed=true}) ->
    {ok, Req};
execute(Task, Req=#msgpack_rpc_request{fsm_ref=FsmRef}) ->
    {msgpack_rpc_request_fsm:execute(Task, FsmRef), Req#msgpack_rpc_request{executed=true}}.

finalize(Req=#msgpack_rpc_request{fsm_ref=FsmRef}) ->
    {msgpack_rpc_request_fsm:finalize(FsmRef), Req}.

response(Error, Result, Req) when is_atom(Error), Error =/= nil ->
    response(msgpack_rpc_protocol:known_error_to_binary(Error), Result, Req);
response(Error, Result, _Req=#msgpack_rpc_request{msg_id=MsgId}) ->
    [?MSGPACK_RPC_RESPONSE, MsgId, Error, Result].

error(Error, Req) ->
    response(Error, nil, Req).

result(Result, Req) ->
    response(nil, Result, Req).

reply(Error, Result, Req=#msgpack_rpc_request{fsm_ref=undefined, transport=Transport, socket=Socket, msgpack_opts=MsgpackOpts}) ->
    Response = response(Error, Result, Req),
    io:format("[~p] RESPONSE: ~p ~p ~n", [?MODULE, Error, Result]),
    Data = msgpack:pack(Response, MsgpackOpts),
    Transport:send(Socket, Data),
    Req;
reply(Error, Result, Req=#msgpack_rpc_request{fsm_ref=FsmRef}) ->
    msgpack_rpc_request_fsm:response(Error, Result, FsmRef),
    Req.
