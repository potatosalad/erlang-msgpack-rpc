%%%-------------------------------------------------------------------
%%% @author UENISHI Kota <kuenishi@gmail.com>
%%% @copyright (C) 2012, UENISHI Kota
%%% @doc
%%%
%%% @end
%%% Created : 24 Jul 2012 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(msgpack_rpc_client).

-include("msgpack_rpc_client.hrl").

%% API
-export([new/3, connect/3, close/1, get/2]).

%% FSM API
-export([start_fsm/1,
         call/3,
         call/4,
         call_async/3,
         call_async/4,
         join/2,
         join/3,
         notify/3,
         reset_stats/1,
         stats/1]).

-define(TIMEOUT, 10000).

-type obj() :: #msgpack_rpc_client{}.
-export_type([obj/0]).

%%%===================================================================
%%% API
%%%===================================================================

% -spec new(inet:ip_address() | inet:hostname(), inet:port_number(), [proplists:property()])
%     -> {ok, pid(), obj()} | ignore | {error, {already_started, pid()} | term()}.
new(Address, Port, Opts) ->
    Client = #msgpack_rpc_client{address=Address, port=Port, options=Opts},
    {ok, parse_options(Client)}.

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

start_fsm(Client) ->
    ranch:require([msgpack_rpc_client]),
    case msgpack_rpc_client_fsm:start(Client) of
        {ok, FsmPid} ->
            {ok, Client#msgpack_rpc_client{fsm_pid=FsmPid}};
        Error ->
            Error
    end.

call(Method, Params, #msgpack_rpc_client{fsm_pid=FsmPid, timeout=Timeout}) ->
    call(FsmPid, Method, Params, Timeout);
call(FsmPid, Method, Params) when is_pid(FsmPid) ->
    call(FsmPid, Method, Params, ?TIMEOUT).

call(Method, Params, Timeout, #msgpack_rpc_client{fsm_pid=FsmPid}) ->
    call(FsmPid, Method, Params, Timeout);
call(FsmPid, Method, Params, Timeout) when is_pid(FsmPid) ->
    case call_async(FsmPid, Method, Params, Timeout) of
        {ok, Req} ->
            join(FsmPid, Req, Timeout);
        Error ->
            Error
    end.

call_async(Method, Params, #msgpack_rpc_client{fsm_pid=FsmPid}) ->
    call_async(FsmPid, Method, Params);
call_async(FsmPid, Method, Params) when is_pid(FsmPid) ->
    msgpack_rpc_client_fsm:request(FsmPid, msgpack_rpc:method_to_binary(Method), Params).

call_async(Method, Params, Timeout, #msgpack_rpc_client{fsm_pid=FsmPid}) ->
    call_async(FsmPid, Method, Params, Timeout);
call_async(FsmPid, Method, Params, Timeout) when is_pid(FsmPid) ->
    msgpack_rpc_client_fsm:request(FsmPid, msgpack_rpc:method_to_binary(Method), Params, Timeout).

join(Req, #msgpack_rpc_client{fsm_pid=FsmPid, timeout=Timeout}) ->
    join(FsmPid, Req, Timeout);
join(FsmPid, Req) when is_pid(FsmPid) ->
    join(FsmPid, Req, ?TIMEOUT).

join(Req, Timeout, #msgpack_rpc_client{fsm_pid=FsmPid}) ->
    join(FsmPid, Req, Timeout);
join(FsmPid, Req, Timeout) when is_pid(FsmPid) ->
    case msgpack_rpc_client_fsm:join(FsmPid, Req, Timeout) of
        {ok, Response} ->
            case msgpack_rpc_response:get([error, result], Response) of
                [nil, Result] ->
                    {ok, Result};
                [Error, nil] ->
                    {error, Error}
            end;
        JoinError ->
            JoinError
    end.

notify(Method, Params, #msgpack_rpc_client{fsm_pid=FsmPid}) ->
    notify(FsmPid, Method, Params);
notify(FsmPid, Method, Params) when is_pid(FsmPid) ->
    msgpack_rpc_client_fsm:notify(FsmPid, msgpack_rpc:method_to_binary(Method), Params).

reset_stats(#msgpack_rpc_client{fsm_pid=FsmPid}) ->
    reset_stats(FsmPid);
reset_stats(FsmPid) when is_pid(FsmPid) ->
    msgpack_rpc_client_fsm:get_and_reset_stats(FsmPid).

stats(#msgpack_rpc_client{fsm_pid=FsmPid}) ->
    stats(FsmPid);
stats(FsmPid) when is_pid(FsmPid) ->
    msgpack_rpc_client_fsm:get_stats(FsmPid).

%%%===================================================================
%%% Connection API
%%%===================================================================

% -spec connect(inet:ip_address() | inet:hostname(), inet:port_number(), [proplists:property()])
%     -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
% connect(Address, Port, Opts) ->
%     ranch:require([msgpack_rpc_client]),
%     msgpack_rpc_connection:start(Address, Port, Opts).

% close(_Client=#msgpack_rpc_client{connection=Pid}) ->
%     close(Pid);
% close(Pid) when is_pid(Pid) ->
%     msgpack_rpc_connection:stop(Pid).

% call(Method, Params, _Client=#msgpack_rpc_client{connection=Pid, timeout=Timeout}) ->
%     call(Pid, Method, Params, Timeout);
% call(Pid, Method, Params) when is_pid(Pid) ->
%     call(Pid, Method, Params, ?DEFAULT_TIMEOUT).

% call(Method, Params, _Client=#msgpack_rpc_client{connection=Pid}, Timeout) ->
%     call(Pid, Method, Params, Timeout);
% call(Pid, Method, Params, Timeout) when is_pid(Pid) ->
%     case call_async(Pid, Method, Params) of
%         {ok, Req} ->
%             join(Pid, Req, Timeout);
%         Error ->
%             Error
%     end.

% call_async(Method, Params, _Client=#msgpack_rpc_client{connection=Pid}) ->
%     call_async(Pid, Method, Params);
% call_async(Pid, Method, Params) when is_pid(Pid) ->
%     msgpack_rpc_connection:request(Pid, msgpack_rpc:method_to_binary(Method), Params).

% join(Req, _Client=#msgpack_rpc_client{connection=Pid, timeout=Timeout}) ->
%     join(Pid, Req, Timeout);
% join(Pid, Req) when is_pid(Pid) ->
%     join(Pid, Req, ?DEFAULT_TIMEOUT).

% join(Req, _Client=#msgpack_rpc_client{connection=Pid}, Timeout) ->
%     join(Pid, Req, Timeout);
% join(Pid, Req, Timeout) when is_pid(Pid) ->
%     case msgpack_rpc_connection:join(Pid, Req, Timeout) of
%         {ok, Response} ->
%             case msgpack_rpc_response:get([error, result], Response) of
%                 [nil, Result] ->
%                     {ok, Result};
%                 [Error, nil] ->
%                     {error, Error}
%             end;
%         JoinError ->
%             JoinError
%     end.

% notify(Method, Params, _Client=#msgpack_rpc_client{connection=Pid}) ->
%     notify(Pid, Method, Params);
% notify(Pid, Method, Params) when is_pid(Pid) ->
%     msgpack_rpc_connection:notify(Pid, msgpack_rpc:method_to_binary(Method), Params).

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

%% @doc Faster alternative to proplists:get_value/3.
%% @private
get_value(Key, Opts, Default) ->
  case lists:keyfind(Key, 1, Opts) of
    {_, Value} -> Value;
    _ -> Default
  end.

%% @private
parse_options(Client=#msgpack_rpc_client{options=Options}) ->
    {ClientOpts, Opts} = msgpack_rpc:partition_options(Options, [timeout]),
    Timeout = get_value(timeout, ClientOpts, Client#msgpack_rpc_client.timeout),
    Client#msgpack_rpc_client{fsm_opts=Opts, timeout=Timeout}.
