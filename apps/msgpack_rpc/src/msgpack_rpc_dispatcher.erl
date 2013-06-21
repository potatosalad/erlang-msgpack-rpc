%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  31 May 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------

-module(msgpack_rpc_dispatcher).

-callback dispatch_init({atom(), msgpack_rpc}, Handler::module(), HandlerOpts::any())
    -> {ok, State}
    | {ok, State, hibernate}
    | {ok, State, timeout()}
    | {ok, State, timeout(), hibernate}
    | {shutdown, Reason::term(), State}.
-callback dispatch_task(Task::msgpack_rpc:task(), State::any())
    -> {ok, Task, State}
    | {ok, Task, State, hibernate}
    | {ok, Task, State, timeout()}
    | {ok, Task, State, timeout(), hibernate}
    | {shutdown, Reason::term(), Task, State}.
-callback dispatch_info(Info::term(), State::any())
    -> {ok, State}
    | {ok, State, hibernate}
    | {ok, State, timeout()}
    | {ok, State, timeout(), hibernate}
    | {shutdown, Reason::term(), State}.
-callback dispatch_terminate(Reason::term(), Task :: undefined | msgpack_rpc:task(), State::any())
    -> term().

%% msgpack_rpc_dispatcher callbacks
-export([dispatch_init/3,
         dispatch_task/2,
         dispatch_info/2,
         dispatch_terminate/3]).

-record(state, {
    type = undefined :: undefined | {atom(), msgpack_rpc},

    %% Handler.
    handler       = undefined :: undefined | module(),
    handler_opts  = undefined :: undefined | any(),
    handler_state = undefined :: undefined | any()
}).

dispatch_init(_Type, undefined, _HandlerOpts) ->
    erlang:error(no_handler_defined);
dispatch_init(Type, Handler, HandlerOpts) ->
    handler_init(#state{type=Type, handler=Handler, handler_opts=HandlerOpts}).

dispatch_task(Task, State) ->
    Msg = msgpack_rpc_task:to_message(Task),
    handler_call(State, msgpack_rpc_handle, Task, Msg).

dispatch_info(Info, State) ->
    handler_info(State, msgpack_rpc_info, Info).

dispatch_terminate(TerminateReason, Task, State) ->
    handler_terminate(State, Task, TerminateReason).

handler_init(State=#state{type=Type, handler=Handler, handler_opts=HandlerOpts}) ->
    case Handler:msgpack_rpc_init(Type, HandlerOpts) of
        {ok, HandlerState} ->
            {ok, State#state{handler_state=HandlerState}};
        {ok, HandlerState, hibernate} ->
            {ok, State#state{handler_state=HandlerState}, hibernate};
        {ok, HandlerState, Timeout} ->
            {ok, State#state{handler_state=HandlerState}, Timeout};
        {ok, HandlerState, Timeout, hibernate} ->
            {ok, State#state{handler_state=HandlerState}, Timeout, hibernate};
        {shutdown, Reason, HandlerState} ->
            {shutdown, Reason, State#state{handler_state=HandlerState}}
    end.

handler_call(State=#state{handler=Handler, handler_opts=_HandlerOpts, handler_state=HandlerState}, Callback, Task, Msg={Type, _, _}) ->
    case Handler:Callback(Msg, Task, HandlerState) of
        {ok, Task2, HandlerState2} ->
            {ok, Task2, State#state{handler_state=HandlerState2}};
        {ok, Task2, HandlerState2, hibernate} ->
            {ok, Task2, State#state{handler_state=HandlerState2}, hibernate};
        {ok, Task2, HandlerState2, Timeout} ->
            {ok, Task2, State#state{handler_state=HandlerState2}, Timeout};
        {ok, Task2, HandlerState2, Timeout, hibernate} ->
            {ok, Task2, State#state{handler_state=HandlerState2}, Timeout, hibernate};
        {execute, Job, Task2, HandlerState2} ->
            msgpack_rpc_task:execute(Job, Task2),
            {ok, Task2, State#state{handler_state=HandlerState2}};
        {execute, Job, Task2, HandlerState2, hibernate} ->
            msgpack_rpc_task:execute(Job, Task2),
            {ok, Task2, State#state{handler_state=HandlerState2}, hibernate};
        {execute, Job, Task2, HandlerState2, Timeout} ->
            msgpack_rpc_task:execute(Job, Task2),
            {ok, Task2, State#state{handler_state=HandlerState2}, Timeout};
        {execute, Job, Task2, HandlerState2, Timeout, hibernate} ->
            msgpack_rpc_task:execute(Job, Task2),
            {ok, Task2, State#state{handler_state=HandlerState2}, Timeout, hibernate};
        {ignore, Task2, HandlerState2} when Type =:= notify ->
            msgpack_rpc_task:ignore(Task2),
            {ok, Task2, State#state{handler_state=HandlerState2}};
        {ignore, Task2, HandlerState2, hibernate} when Type =:= notify ->
            msgpack_rpc_task:ignore(Task2),
            {ok, Task2, State#state{handler_state=HandlerState2}, hibernate};
        {ignore, Task2, HandlerState2, Timeout} when Type =:= notify ->
            msgpack_rpc_task:ignore(Task2),
            {ok, Task2, State#state{handler_state=HandlerState2}, Timeout};
        {ignore, Task2, HandlerState2, Timeout, hibernate} when Type =:= notify ->
            msgpack_rpc_task:ignore(Task2),
            {ok, Task2, State#state{handler_state=HandlerState2}, Timeout, hibernate};
        {respond, Resp, Task2, HandlerState2} when Type =:= request ->
            msgpack_rpc_task:respond(Resp, Task2),
            {ok, Task2, State#state{handler_state=HandlerState2}};
        {respond, Resp, Task2, HandlerState2, hibernate} when Type =:= request ->
            msgpack_rpc_task:respond(Resp, Task2),
            {ok, Task2, State#state{handler_state=HandlerState2}, hibernate};
        {respond, Resp, Task2, HandlerState2, Timeout} when Type =:= request ->
            msgpack_rpc_task:respond(Resp, Task2),
            {ok, Task2, State#state{handler_state=HandlerState2}, Timeout};
        {respond, Resp, Task2, HandlerState2, Timeout, hibernate} when Type =:= request ->
            msgpack_rpc_task:respond(Resp, Task2),
            {ok, Task2, State#state{handler_state=HandlerState2}, Timeout, hibernate};
        {shutdown, Reason, Task2, HandlerState2} ->
            {shutdown, Reason, Task2, State#state{handler_state=HandlerState2}}
    end.

handler_info(State=#state{handler=Handler, handler_opts=_HandlerOpts, handler_state=HandlerState}, Callback, Info) ->
    case Handler:Callback(Info, HandlerState) of
        {ok, HandlerState2} ->
            {ok, State#state{handler_state=HandlerState2}};
        {ok, HandlerState2, hibernate} ->
            {ok, State#state{handler_state=HandlerState2}, hibernate};
        {ok, HandlerState2, Timeout} ->
            {ok, State#state{handler_state=HandlerState2}, Timeout};
        {ok, HandlerState2, Timeout, hibernate} ->
            {ok, State#state{handler_state=HandlerState2}, Timeout, hibernate};
        {shutdown, Reason, HandlerState2} ->
            {shutdown, Reason, State#state{handler_state=HandlerState2}}
    end.

handler_terminate(#state{handler=Handler, handler_opts=_HandlerOpts, handler_state=HandlerState}, Task, Reason) ->
    Handler:msgpack_rpc_terminate(Reason, Task, HandlerState).
