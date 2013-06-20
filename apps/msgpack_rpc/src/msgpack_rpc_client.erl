%%%-------------------------------------------------------------------
%%% @author UENISHI Kota <kuenishi@gmail.com>
%%% @copyright (C) 2012, UENISHI Kota
%%% @doc
%%%
%%% @end
%%% Created : 24 Jul 2012 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(msgpack_rpc_client).

-include("msgpack_rpc.hrl").
-include("msgpack_rpc_client.hrl").

%% API
-export([new/3, connect/3, close/1, get/2]).

%% FSM API
-export([start_fsm/1]).
-export([call/3, call/4,
         call_async/3, call_async/4,
         join/2, join/3,
         notify/3,
         reset_stats/1, stats/1]).

%% Internal
-import(msgpack_rpc_util, [get_value/3]).

-type obj() :: #msgpack_rpc_client{}.
-export_type([obj/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(inet:ip_address() | inet:hostname(), inet:port_number(), [proplists:property()])
    -> {ok, obj()}.
new(Address, Port, Opts) ->
    Client = #msgpack_rpc_client{address=Address, port=Port, options=Opts},
    {ok, parse_options(Client)}.

-spec connect(inet:ip_address() | inet:hostname(), inet:port_number(), [proplists:property()])
    -> {ok, obj()} | ignore | {error, {already_started, pid()} | term()}.
connect(Address, Port, Opts) ->
    {ok, Client} = new(Address, Port, Opts),
    start_fsm(Client).

close(#msgpack_rpc_client{fsm_pid=FsmPid}) ->
    close(FsmPid);
close(FsmPid) when is_pid(FsmPid) ->
    msgpack_rpc_client_fsm:shutdown(FsmPid).

-spec get(list(atom()) | atom(), obj()) -> list(term()) | term().
get(List, Obj) when is_list(List) ->
    [g(Atom, Obj) || Atom <- List];
get(Atom, Obj) when is_atom(Atom) ->
    g(Atom, Obj).

%%====================================================================
%% FSM API functions
%%====================================================================

-spec start_fsm(msgpack_rpc:client())
    -> {ok, msgpack_rpc:client()} | {error, term()}.
start_fsm(Client) ->
    ranch:require([msgpack_rpc_client]),
    case msgpack_rpc_client_fsm:start(Client) of
        {ok, FsmPid} ->
            {ok, Client#msgpack_rpc_client{fsm_pid=FsmPid}};
        Error ->
            Error
    end.

-spec call(msgpack_rpc:method(), msgpack_rpc:params(), msgpack_rpc:client())
    -> {ok, msgpack_rpc:result()} | {error, msgpack_rpc:error()}.
call(Method, Params, Client=#msgpack_rpc_client{timeout=Timeout}) ->
    call(Method, Params, Timeout, Client).

-spec call(msgpack_rpc:method(), msgpack_rpc:params(), timeout(), msgpack_rpc:client())
    -> {ok, msgpack_rpc:result()} | {error, msgpack_rpc:error()}.
call(Method, Params, Timeout, #msgpack_rpc_client{fsm_pid=FsmPid}) ->
    case msgpack_rpc_client_fsm:request_join(FsmPid, {msgpack_rpc_util:method_to_binary(Method), Params, Timeout}) of
        {ok, Response} ->
            parse_response(Response);
        Error ->
            Error
    end.

-spec call_async(msgpack_rpc:method(), msgpack_rpc:params(), msgpack_rpc:client())
    -> {ok, msgpack_rpc:request()} | {error, term()}.
call_async(Method, Params, Client=#msgpack_rpc_client{timeout=Timeout}) ->
    call_async(Method, Params, Timeout, Client).

-spec call_async(msgpack_rpc:method(), msgpack_rpc:params(), timeout(), msgpack_rpc:client())
    -> {ok, msgpack_rpc:request()} | {error, term()}.
call_async(Method, Params, Timeout, #msgpack_rpc_client{fsm_pid=FsmPid}) ->
    msgpack_rpc_client_fsm:request(FsmPid, {msgpack_rpc_util:method_to_binary(Method), Params, Timeout}).

-spec join(msgpack_rpc:request(), msgpack_rpc:client())
    -> {ok, msgpack_rpc:result()} | {error, msgpack_rpc:error()}.
join(Req, Client=#msgpack_rpc_client{timeout=Timeout}) ->
    join(Req, Timeout, Client).

-spec join(msgpack_rpc:request(), timeout(), msgpack_rpc:client())
    -> {ok, msgpack_rpc:result()} | {error, msgpack_rpc:error()}.
join(Req, Timeout, #msgpack_rpc_client{fsm_pid=FsmPid}) ->
    case msgpack_rpc_client_fsm:join(FsmPid, {Req, Timeout}) of
        {ok, Response} ->
            parse_response(Response);
        Error ->
            Error
    end.

-spec notify(msgpack_rpc:method(), msgpack_rpc:params(), msgpack_rpc:client())
    -> ok.
notify(Method, Params, #msgpack_rpc_client{fsm_pid=FsmPid}) ->
    msgpack_rpc_client_fsm:notify(FsmPid, {msgpack_rpc_util:method_to_binary(Method), Params}).

-spec reset_stats(msgpack_rpc:client())
    -> {ok, [{binary(), integer(), integer()}]}.
reset_stats(#msgpack_rpc_client{fsm_pid=FsmPid}) ->
    msgpack_rpc_client_fsm:get_and_reset_stats(FsmPid).

-spec stats(msgpack_rpc:client())
    -> {ok, [{binary(), integer(), integer()}]}.
stats(#msgpack_rpc_client{fsm_pid=FsmPid}) ->
    msgpack_rpc_client_fsm:get_stats(FsmPid).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
g(address, #msgpack_rpc_client{address=Ret}) -> Ret;
g(port, #msgpack_rpc_client{port=Ret}) -> Ret;
g(options, #msgpack_rpc_client{options=Ret}) -> Ret;
g(fsm_opts, #msgpack_rpc_client{fsm_opts=Ret}) -> Ret;
g(fsm_pid, #msgpack_rpc_client{fsm_pid=Ret}) -> Ret;
g(timeout, #msgpack_rpc_client{timeout=Ret}) -> Ret.

%% @private
parse_options(Client=#msgpack_rpc_client{options=Options}) ->
    {ClientOpts, Opts} = msgpack_rpc_util:partition_options(Options, [timeout]),
    Timeout = get_value(timeout, ClientOpts, Client#msgpack_rpc_client.timeout),
    Client#msgpack_rpc_client{fsm_opts=Opts, timeout=Timeout}.

%% @private
parse_response(#msgpack_rpc_response{error=nil, result=Result}) ->
    {ok, Result};
parse_response(#msgpack_rpc_response{error=Error, result=nil}) ->
    {error, Error}.
