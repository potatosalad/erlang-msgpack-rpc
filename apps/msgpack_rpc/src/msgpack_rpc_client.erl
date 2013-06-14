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
-export([connect/3,
         connection/1,
         close/1,
         call/3,
         call/4,
         call_async/3,
         call_async/4,
         join/2,
         join/3,
         notify/3,
         notify/4]).

%%%===================================================================
%%% API
%%%===================================================================

connect(Address, Port, Opts) ->
    Client = #msgpack_rpc_client{},
    {ClientOpts, Opts2} = msgpack_rpc:partition_options(Opts, [timeout]),
    Timeout = proplists:get_value(timeout, ClientOpts, Client#msgpack_rpc_client.timeout),
    case msgpack_rpc_connection:start_link(Address, Port, Opts2) of
        {ok, Pid} ->
            {ok, Client#msgpack_rpc_client{connection=Pid, timeout=Timeout}};
        Error ->
            Error
    end.

connection(_Client=#msgpack_rpc_client{connection=Pid}) ->
    connection(Pid);
connection(Pid) when is_pid(Pid) ->
    Pid.

close(_Client=#msgpack_rpc_client{connection=Pid}) ->
    close(Pid);
close(Pid) when is_pid(Pid) ->
    msgpack_rpc_connection:stop(Pid).

call(Method, Params, _Client=#msgpack_rpc_client{connection=Pid, timeout=Timeout}) ->
    call(Pid, Method, Params, Timeout);
call(Pid, Method, Params) when is_pid(Pid) ->
    case call_async(Pid, Method, Params) of
        {ok, Req} ->
            join(Pid, Req);
        Error ->
            Error
    end.

call(Method, Params, _Client=#msgpack_rpc_client{connection=Pid}, Timeout) ->
    call(Pid, Method, Params, Timeout);
call(Pid, Method, Params, Timeout) when is_pid(Pid) ->
    case call_async(Pid, Method, Params, Timeout) of
        {ok, Req} ->
            join(Pid, Req, Timeout);
        Error ->
            Error
    end.

call_async(Method, Params, _Client=#msgpack_rpc_client{connection=Pid, timeout=Timeout}) ->
    call_async(Pid, Method, Params, Timeout);
call_async(Pid, Method, Params) when is_pid(Pid) ->
    msgpack_rpc_connection:request(Pid, msgpack_rpc:method_to_binary(Method), Params).

call_async(Method, Params, _Client=#msgpack_rpc_client{connection=Pid}, Timeout) ->
    call_async(Pid, Method, Params, Timeout);
call_async(Pid, Method, Params, Timeout) when is_pid(Pid) ->
    msgpack_rpc_connection:request(Pid, msgpack_rpc:method_to_binary(Method), Params, Timeout).

join(Req, _Client=#msgpack_rpc_client{connection=Pid, timeout=Timeout}) ->
    join(Pid, Req, Timeout);
join(Pid, Req) when is_pid(Pid) ->
    case msgpack_rpc_connection:join(Pid, Req) of
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

join(Req, _Client=#msgpack_rpc_client{connection=Pid}, Timeout) ->
    join(Pid, Req, Timeout);
join(Pid, Req, Timeout) when is_pid(Pid) ->
    case msgpack_rpc_connection:join(Pid, Req, Timeout) of
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

notify(Method, Params, _Client=#msgpack_rpc_client{connection=Pid, timeout=Timeout}) ->
    notify(Pid, Method, Params, Timeout);
notify(Pid, Method, Params) when is_pid(Pid) ->
    case msgpack_rpc_connection:notify(Pid, msgpack_rpc:method_to_binary(Method), Params) of
        {ok, _} ->
            ok;
        Error ->
            Error
    end.

notify(Method, Params, _Client=#msgpack_rpc_client{connection=Pid}, Timeout) ->
    notify(Pid, Method, Params, Timeout);
notify(Pid, Method, Params, Timeout) when is_pid(Pid) ->
    case msgpack_rpc_connection:notify(Pid, msgpack_rpc:method_to_binary(Method), Params, Timeout) of
        {ok, _} ->
            ok;
        Error ->
            Error
    end.
