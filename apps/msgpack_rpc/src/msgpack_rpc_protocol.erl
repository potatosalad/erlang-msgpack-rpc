%%%-------------------------------------------------------------------
%%% @author UENISHI Kota <kuenishi@gmail.com>
%%% @copyright (C) 2012, UENISHI Kota
%%% @doc
%%%
%%% @end
%%% Created : 22 Jul 2012 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(msgpack_rpc_protocol).
-behaviour(ranch_protocol).

-include("msgpack_rpc.hrl").
-include("msgpack_rpc_server.hrl").

% -ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
% -endif.

%% ranch_protocol callbacks
-export([start_link/4]).

%% Internal.
-export([init/1]).
-export([loop/3]).
-import(msgpack_rpc_util, [get_value/3]).

-record(state, {
    %% Settings
    socket    = undefined :: undefined | inet:socket(),
    transport = undefined :: undefined | module(),
    opts      = undefined :: undefined | [proplists:property()],

    %% Options
    options  = #msgpack_rpc_options{} :: msgpack_rpc:options(),
    dispatch = undefined :: undefined | module(),
    timeout  = undefined :: undefined | timeout(),

    %% Other
    hibernate   = false     :: boolean(),
    messages    = undefined :: undefined | {atom(), atom(), atom()},
    timeout_ref = undefined :: undefined | reference()
}).

%%%===================================================================
%%% ranch_protocol callbacks
%%%===================================================================

%% @doc Start an msgpack_rpc_protocol process.
-spec start_link(ranch:ref(), inet:socket(), module(), [proplists:property()])
    -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
    ranch:require([ranch, crypto, msgpack_rpc_server]),
    Pid = spawn_link(?MODULE, init, [[Ref, Socket, Transport, Opts]]),
    {ok, Pid}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

init([Ref, Socket, Transport, Opts]) ->
    {ProtoOpts, Opts2} = msgpack_rpc_util:partition_options(Opts, [dispatcher, handler, handler_opts, timeout]),
    Dispatch = get_value(dispatcher, ProtoOpts, msgpack_rpc_dispatcher),
    Handler = get_value(handler, ProtoOpts, undefined),
    HandlerOpts = get_value(handler_opts, ProtoOpts, []),
    Timeout = get_value(timeout, ProtoOpts, infinity),
    Options = msgpack_rpc_options:new(Opts2),
    ok = ranch:accept_ack(Ref),
    dispatcher_init(#state{socket=Socket, transport=Transport, opts=Opts,
        options=Options, dispatch=Dispatch, timeout=Timeout,
        messages=Transport:messages()}, Handler, HandlerOpts).

dispatcher_init(State=#state{transport=Transport, dispatch=Dispatch}, Handler, HandlerOpts) ->
    try Dispatch:dispatch_init({Transport:name(), msgpack_rpc}, Handler, HandlerOpts) of
        {ok, DispatchState} ->
            before_loop(State, DispatchState, <<>>);
        {ok, DispatchState, hibernate} ->
            before_loop(State#state{hibernate=true}, DispatchState, <<>>);
        {ok, DispatchState, Timeout} ->
            before_loop(State#state{timeout=Timeout}, DispatchState, <<>>);
        {ok, DispatchState, Timeout, hibernate} ->
            before_loop(State#state{timeout=Timeout, hibernate=true}, DispatchState, <<>>);
        {shutdown, Reason, DispatchState} ->
            dispatcher_terminate(Reason, DispatchState, undefined, State)
    catch
        Class:Reason ->
            error_logger:error_msg(
                "** ~p ~p terminating in ~p/~p~n"
                "   for the reason ~p:~p~n** Options were ~p~n"
                "** Stacktrace: ~p~n~n",
                [?MODULE, self(), dispatcher_init, 1, Class, Reason, State#state.options, erlang:get_stacktrace()]),
            terminate(Reason, undefined)
    end.

dispatcher_task(State=#state{dispatch=Dispatch}, DispatchState, Data, Callback, Task, NextState) ->
    try Dispatch:Callback(Task, DispatchState) of
        {ok, _Task2, DispatchState2} ->
            State2 = loop_timeout(State),
            NextState(State2, DispatchState2, Data);
        {ok, _Task2, DispatchState2, hibernate} ->
            State2 = loop_timeout(State),
            NextState(State2#state{hibernate=true}, DispatchState2, Data);
        {ok, _Task2, DispatchState2, Timeout} ->
            State2 = loop_timeout(State),
            NextState(State2#state{timeout=Timeout}, DispatchState2, Data);
        {ok, _Task2, DispatchState2, Timeout, hibernate} ->
            State2 = loop_timeout(State),
            NextState(State2#state{timeout=Timeout, hibernate=true}, DispatchState2, Data);
        {shutdown, Reason, Task2, DispatchState2} ->
            dispatcher_terminate(State, DispatchState2, Task2, Reason)
    catch
        Class:Reason ->
            error_logger:error_msg(
                "** ~p ~p terminating in ~p/~p~n"
                "   for the reason ~p:~p~n** Options were ~p~n"
                "** Stacktrace: ~p~n~n",
                [?MODULE, self(), dispatcher_task, 6, Class, Reason, State#state.options, erlang:get_stacktrace()]),
            dispatcher_terminate(State, DispatchState, Task, Reason)
    end.

dispatcher_info(State=#state{dispatch=Dispatch}, DispatchState, Data, Callback, Info, NextState) ->
    try Dispatch:Callback(Info, DispatchState) of
        {ok, DispatchState2} ->
            State2 = loop_timeout(State),
            NextState(State2, DispatchState2, Data);
        {ok, DispatchState2, hibernate} ->
            State2 = loop_timeout(State),
            NextState(State2#state{hibernate=true}, DispatchState2, Data);
        {ok, DispatchState2, Timeout} ->
            State2 = loop_timeout(State),
            NextState(State2#state{timeout=Timeout}, DispatchState2, Data);
        {ok, DispatchState2, Timeout, hibernate} ->
            State2 = loop_timeout(State),
            NextState(State2#state{timeout=Timeout, hibernate=true}, DispatchState2, Data);
        {shutdown, Reason, DispatchState2} ->
            dispatcher_terminate(State, DispatchState2, undefined, Reason)
    catch
        Class:Reason ->
            error_logger:error_msg(
                "** ~p ~p terminating in ~p/~p~n"
                "   for the reason ~p:~p~n** Options were ~p~n"
                "** Stacktrace: ~p~n~n",
                [?MODULE, self(), dispatcher_info, 6, Class, Reason, State#state.options, erlang:get_stacktrace()]),
            dispatcher_terminate(State, DispatchState, undefined, Reason)
    end.

dispatcher_terminate(State=#state{dispatch=Dispatch}, DispatchState, Task, TerminateReason) ->
    try
        Dispatch:dispatch_terminate(TerminateReason, Task, DispatchState)
    catch
        Class:Reason ->
            error_logger:error_msg(
                "** ~p ~p terminating in ~p/~p~n"
                "   for the reason ~p:~p~n** Options were ~p~n"
                "** Task was ~p~n** Stacktrace: ~p~n~n",
                [?MODULE, self(), dispatcher_terminate, 4, Class, Reason, State#state.options, Task, erlang:get_stacktrace()]),
            terminate(Reason, State)
    end.

loop_timeout(State=#state{timeout=infinity}) ->
    State#state{timeout_ref=undefined};
loop_timeout(State=#state{timeout=Timeout, timeout_ref=PrevRef}) ->
    _ = case PrevRef of
        undefined -> ignore;
        PrevRef -> erlang:cancel_timer(PrevRef)
    end,
    TRef = erlang:start_timer(Timeout, self(), ?MODULE),
    State#state{timeout_ref=TRef}.

before_loop(State=#state{hibernate=true, transport=Transport, socket=Socket}, DispatchState, SoFar) ->
    Transport:setopts(Socket, [{active, once}]),
    erlang:hibernate(?MODULE, loop, [State#state{hibernate=false}, DispatchState, SoFar]);
before_loop(State=#state{transport=Transport, socket=Socket}, DispatchState, SoFar) ->
    Transport:setopts(Socket, [{active, once}]),
    loop(State, DispatchState, SoFar).

loop(State=#state{socket=Socket, messages={OK, Closed, Error}, timeout_ref=TRef}, DispatchState, SoFar) ->
    receive
        {OK, Socket, Data} ->
            State2 = loop_timeout(State),
            parse_data(State2, DispatchState, << SoFar/binary, Data/binary >>);
        {Closed, Socket} ->
            dispatcher_terminate(State, DispatchState, undefined, {error, closed});
        {Error, Socket, Reason} ->
            dispatcher_terminate(State, DispatchState, undefined, {error, Reason});
        {timeout, TRef, ?MODULE} ->
            dispatcher_terminate(State, DispatchState, undefined, {normal, timeout});
        {timeout, OlderTRef, ?MODULE} when is_reference(OlderTRef) ->
            loop(State, DispatchState, SoFar);
        Message ->
            dispatcher_info(State, DispatchState, SoFar, dispatch_info, Message, fun loop/3)
    end.

parse_data(State=#state{transport=Transport, socket=Socket, options=Options=#msgpack_rpc_options{
        msgpack_unpacker=Unpacker}}, DispatchState, Data) ->
    case Unpacker(Data) of
        {[?MSGPACK_RPC_REQUEST, MsgId, Method, Params], RemainingData} ->
            case msgpack_rpc_task:start_fsm({MsgId, Method, Params, Socket, Transport, Options}) of
                {ok, Task} ->
                    dispatcher_task(State, DispatchState, RemainingData, dispatch_task, Task, fun parse_data/3);
                {error, Reason} ->
                    dispatcher_terminate(State, DispatchState, undefined, Reason)
            end;
        {[?MSGPACK_RPC_NOTIFY, Method, Params], RemainingData} ->
            case msgpack_rpc_task:start_fsm({Method, Params}) of
                {ok, Task} ->
                    dispatcher_task(State, DispatchState, RemainingData, dispatch_task, Task, fun parse_data/3);
                {error, Reason} ->
                    dispatcher_terminate(State, DispatchState, undefined, Reason)
            end;
        {error, incomplete} ->
            %% Need more data.
            before_loop(State, DispatchState, Data);
        {error, _} = Error ->
            dispatcher_terminate(State, DispatchState, undefined, Error)
    end.

terminate(_TerminateReason, _State=#state{transport=Transport, socket=Socket}) ->
    Transport:close(Socket),
    ok.
