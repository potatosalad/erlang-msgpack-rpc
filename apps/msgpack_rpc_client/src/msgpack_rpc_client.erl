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
-export([connect/4,
         connection/1,
         close/1,
         call/3,
         call_async/3,
         join/2,
         notify/3]).

%%%===================================================================
%%% API
%%%===================================================================

connect(Transport, Address, Port, Opts) ->
    case msgpack_rpc_connection:start_link(Transport, Address, Port, Opts) of
        {ok, Pid} ->
            {ok, #msgpack_rpc_client{connection=Pid}};
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

call(Method, Params, _Client=#msgpack_rpc_client{connection=Pid}) ->
    call(Pid, Method, Params);
call(Pid, Method, Params) when is_pid(Pid) ->
    case call_async(Pid, Method, Params) of
        {ok, Req} ->
            join(Pid, Req);
        Error ->
            Error
    end.

call_async(Method, Params, _Client=#msgpack_rpc_client{connection=Pid}) ->
    call_async(Pid, Method, Params);
call_async(Pid, Method, Params) when is_pid(Pid) ->
    msgpack_rpc_connection:request(Pid, msgpack_rpc:method_to_binary(Method), Params).

join(Req, _Client=#msgpack_rpc_client{connection=Pid}) ->
    join(Pid, Req);
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

notify(Method, Params, _Client=#msgpack_rpc_client{connection=Pid}) ->
    notify(Pid, Method, Params);
notify(Pid, Method, Params) when is_pid(Pid) ->
    case msgpack_rpc_connection:notify(Pid, msgpack_rpc:method_to_binary(Method), Params) of
        {ok, _} ->
            ok;
        Error ->
            Error
    end.
