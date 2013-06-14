%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  11 June 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------

-module(msgpack_rpc_task).

-include("msgpack_rpc.hrl").
-include("msgpack_rpc_server.hrl").

%% API
-export([new/1,
         get/2,
         to_req/1]).

%% FSM API
-export([start_fsm/1,
         execute/2,
         respond/2,
         shutdown/1]).

-type req() :: {notify | request, msgpack_rpc:method(), msgpack_rpc:params()}.
-export_type([req/0]).

-type resp() :: {result | error, nil | msgpack:object()}.
-export_type([resp/0]).

%%====================================================================
%% API functions
%%====================================================================

new([Method, Params]) ->
    new([Method, Params, #msgpack_rpc_options{}]);
new([Method, Params, Opts]) ->
    Message = msgpack_rpc_notify:new(Method, Params),
    Task = #msgpack_rpc_task{type=notify, message=Message, options=Opts},
    {ok, Task};
new([MsgId, Method, Params, Socket, Transport]) ->
    new([MsgId, Method, Params, Socket, Transport, #msgpack_rpc_options{}]);
new([MsgId, Method, Params, Socket, Transport, Opts]) ->
    Message = msgpack_rpc_request:new(MsgId, Method, Params),
    Task = #msgpack_rpc_task{type=request, message=Message, socket=Socket,
        transport=Transport, options=Opts},
    io:format("Request Task: ~p~n", [Task]),
    {ok, Task}.

get(List, Task) when is_list(List) ->
    [g(Atom, Task) || Atom <- List];
get(Atom, Task) when is_atom(Atom) ->
    g(Atom, Task).

to_req(Task) ->
    [Type, Method, Params] = get([type, message_method, message_params], Task),
    {Type, Method, Params}.

%%====================================================================
%% FSM API functions
%%====================================================================

start_fsm(Args) ->
    {ok, Task} = new(Args),
    start_fsm_(Task).

execute(Job, #msgpack_rpc_task{fsm_pid=FsmPid}) when is_pid(FsmPid) ->
    msgpack_rpc_task_fsm:execute(FsmPid, Job).

respond(Resp, #msgpack_rpc_task{fsm_pid=FsmPid}) when is_pid(FsmPid) ->
    msgpack_rpc_task_fsm:respond(FsmPid, Resp).

shutdown(#msgpack_rpc_task{fsm_pid=FsmPid}) when is_pid(FsmPid) ->
    msgpack_rpc_task_fsm:shutdown(FsmPid).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

g(type, #msgpack_rpc_task{type=Ret}) -> Ret;
g(message, #msgpack_rpc_task{message=Ret}) -> Ret;
g(message_method, #msgpack_rpc_task{type=notify, message=#msgpack_rpc_notify{method=Ret}}) -> Ret;
g(message_params, #msgpack_rpc_task{type=notify, message=#msgpack_rpc_notify{params=Ret}}) -> Ret;
g(message_method, #msgpack_rpc_task{type=request, message=#msgpack_rpc_request{method=Ret}}) -> Ret;
g(message_params, #msgpack_rpc_task{type=request, message=#msgpack_rpc_request{params=Ret}}) -> Ret;
g(options, #msgpack_rpc_task{options=Ret}) -> Ret;
g(fsm_pid, #msgpack_rpc_task{fsm_pid=Ret}) -> Ret;
g(response, #msgpack_rpc_task{response=Ret}) -> Ret;
g(socket, #msgpack_rpc_task{socket=Ret}) -> Ret;
g(transport, #msgpack_rpc_task{transport=Ret}) -> Ret.

start_fsm_(Task) ->
    case msgpack_rpc_task_fsm:start(Task) of
        {ok, FsmPid} ->
            {ok, Task#msgpack_rpc_task{fsm_pid=FsmPid}};
        Error ->
            Error
    end.
