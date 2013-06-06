%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  31 May 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------

-module(msgpack_rpc_handler).
-behaviour(msgpack_rpc_middleware).

-callback init({atom(), msgpack_rpc}, State::any())
    -> {ok, State}
    | {ok, State, hibernate}
    | {ok, State, timeout()}
    | {ok, State, timeout(), hibernate}
    | {shutdown, State}.
-callback handle_request(Req::msgpack_rpc_request:req(), State::any())
    -> {ok, Req, State}
    | {ok, Req, State, hibernate}
    | {ok, Req, State, timeout()}
    | {ok, Req, State, timeout(), hibernate}
    | {shutdown, Req, State}.
-callback handle_notify(Req::msgpack_rpc_notify:req(), State::any())
    -> {ok, Req, State}
    | {ok, Req, State, hibernate}
    | {ok, Req, State, timeout()}
    | {ok, Req, State, timeout(), hibernate}
    | {shutdown, Req, State}.
-callback handle_info(Info::term(), State::any())
    -> {ok, State}
    | {ok, State, hibernate}
    | {ok, State, timeout()}
    | {ok, State, timeout(), hibernate}
    | {shutdown, State}.
-callback terminate(Reason::term(), Req :: undefined | msgpack_rpc_notify:req() | msgpack_rpc_request:req(), State::any())
    -> term().

%% msgpack_rpc_middleware callbacks
-export([init/2,
         request/2,
         notify/2,
         info/2,
         terminate/3]).

-record(state, {
    transport     = undefined :: undefined | any(),
    handler       = undefined :: undefined | module(),
    handler_opts  = undefined :: undefined | any(),
    handler_state = undefined :: undefined | any()
}).

init(Transport, Env) ->
    {_, Handler}     = lists:keyfind(handler, 1, Env),
    {_, HandlerOpts} = lists:keyfind(handler_opts, 1, Env),
    handler_init(#state{
        transport=Transport,
        handler=Handler,
        handler_opts=HandlerOpts}, Env).

request(Req, Env) ->
    State = get_state(Env),
    handler_execute(State, handle_request, Req, Env).

notify(Req, Env) ->
    State = get_state(Env),
    handler_execute(State, handle_notify, Req, Env).

info(Info, Env) ->
    State = get_state(Env),
    handler_info(State, handle_info, Info, Env).

terminate(Reason, Req, Env) ->
    State = get_state(Env),
    handler_terminate(State, Reason, Req, Env).

handler_init(State=#state{transport=Transport, handler=Handler, handler_opts=HandlerOpts}, Env) ->
    try Handler:init(Transport, HandlerOpts) of
        {ok, HandlerState} ->
            {ok, set_state(State#state{handler_state=HandlerState}, Env)};
        {ok, HandlerState, hibernate} ->
            {ok, set_state(State#state{handler_state=HandlerState}, Env), hibernate};
        {ok, HandlerState, Timeout} ->
            {ok, set_state(State#state{handler_state=HandlerState}, Env), Timeout};
        {ok, HandlerState, Timeout, hibernate} ->
            {ok, set_state(State#state{handler_state=HandlerState}, Env), Timeout, hibernate};
        {shutdown, HandlerState} ->
            {shutdown, set_state(State#state{handler_state=HandlerState}, Env)};
        Other ->
            Other
    catch Class:Reason ->
        error_logger:error_msg(
            "** msgpack_rpc handler ~p terminating in ~p/~p~n"
            "   for the reason ~p:~p~n** Options were ~p~n"
            "** Env was: ~p~n** Stacktrace: ~p~n~n",
            [Handler, init, 2, Class, Reason, HandlerOpts, Env, erlang:get_stacktrace()]),
        {shutdown, Env}
    end.

handler_execute(State=#state{handler=Handler, handler_opts=HandlerOpts, handler_state=HandlerState}, Callback, Req, Env) ->
    try Handler:Callback(Req, HandlerState) of
        {ok, Req2, HandlerState2} ->
            {ok, Req2, set_state(State#state{handler_state=HandlerState2}, Env)};
        {ok, Req2, HandlerState2, hibernate} ->
            {ok, Req2, set_state(State#state{handler_state=HandlerState2}, Env), hibernate};
        {ok, Req2, HandlerState2, Timeout} ->
            {ok, Req2, set_state(State#state{handler_state=HandlerState2}, Env), Timeout};
        {ok, Req2, HandlerState2, Timeout, hibernate} ->
            {ok, Req2, set_state(State#state{handler_state=HandlerState2}, Env), Timeout, hibernate};
        {shutdown, Req2, HandlerState2} ->
            {shutdown, Req2, set_state(State#state{handler_state=HandlerState2}, Env)};
        Other ->
            Other
    catch Class:Reason ->
        error_logger:error_msg(
            "** msgpack_rpc handler ~p terminating in ~p/~p~n"
            "   for the reason ~p:~p~n** Options were ~p~n"
            "** Request was ~p~n** Env was: ~p~n"
            "** Stacktrace: ~p~n~n",
            [Handler, Callback, 2, Class, Reason, HandlerOpts, Req, Env, erlang:get_stacktrace()]),
        {shutdown, Req, Env}
    end.

handler_info(State=#state{handler=Handler, handler_opts=HandlerOpts, handler_state=HandlerState}, Callback, Info, Env) ->
    try Handler:Callback(Info, HandlerState) of
        {ok, HandlerState2} ->
            {ok, set_state(State#state{handler_state=HandlerState2}, Env)};
        {ok, HandlerState2, hibernate} ->
            {ok, set_state(State#state{handler_state=HandlerState2}, Env), hibernate};
        {ok, HandlerState2, Timeout} ->
            {ok, set_state(State#state{handler_state=HandlerState2}, Env), Timeout};
        {ok, HandlerState2, Timeout, hibernate} ->
            {ok, set_state(State#state{handler_state=HandlerState2}, Env), Timeout, hibernate};
        {shutdown, HandlerState2} ->
            {shutdown, set_state(State#state{handler_state=HandlerState2}, Env)};
        Other ->
            Other
    catch Class:Reason ->
        error_logger:error_msg(
            "** msgpack_rpc handler ~p terminating in ~p/~p~n"
            "   for the reason ~p:~p~n** Options were ~p~n"
            "** Info was ~p~n** Env was: ~p~n"
            "** Stacktrace: ~p~n~n",
            [Handler, Callback, 2, Class, Reason, HandlerOpts, Info, Env, erlang:get_stacktrace()]),
        {shutdown, Env}
    end.

handler_terminate(#state{handler=Handler, handler_opts=HandlerOpts, handler_state=HandlerState}, TerminateReason, Req, _Env) ->
    try
        Handler:terminate(TerminateReason, Req, HandlerState)
    catch Class:Reason ->
        error_logger:error_msg(
            "** msgpack_rpc handler ~p terminating in ~p/~p~n"
            "   for the reason ~p:~p~n** Initial reason was ~p~n"
            "** Options were ~p~n** Handler state was ~p~n"
            "** Request was ~p~n** Stacktrace: ~p~n~n",
            [Handler, terminate, 3, Class, Reason, TerminateReason, HandlerOpts, HandlerState, Req, erlang:get_stacktrace()])
    end.

get_state(Env) ->
    msgpack_rpc_middleware:get_state(?MODULE, Env).

set_state(State, Env) ->
    msgpack_rpc_middleware:set_state(?MODULE, State, Env).
