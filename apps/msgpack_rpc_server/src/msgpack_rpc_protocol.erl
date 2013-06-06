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

-include("msgpack_rpc_server.hrl").

% -ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
% -endif.

%% ranch_protocol callbacks
-export([start_link/4]).

%% Internal.
-export([init/4]).
% -export([parse_request/2]).
-export([loop/3]).

-export([known_error_to_binary/1]).
-export([binary_to_known_error/1]).

-record(state, {
    socket       = undefined :: undefined | inet:socket(),
    transport    = undefined :: undefined | module(),
    middlewares  = undefined :: undefined | [module()],
    env          = undefined :: undefined | msgpack_rpc_middleware:env(),
    % protocol     = undefined :: undefined | pid(),
    % handler      = undefined :: undefined | module(),
    % handler_opts = undefined :: any(),
    msgpack_opts = undefined :: undefined | any(),
    messages     = undefined :: undefined | {atom(), atom(), atom()},
    timeout      = undefined :: undefined | timeout(),
    timeout_ref  = undefined :: undefined | reference(),
    hibernate    = false     :: boolean()
}).

%% ranch_protocol callbacks

%% @doc Start an msgpack_rpc_protocol process.
% -spec start_link(ranch:ref(), inet:socket(), module(), [any(), module(), any()])
%     -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

%% Internal.

%% @doc Faster alternative to proplists:get_value/3.
%% @private
get_value(Key, Opts, Default) ->
  case lists:keyfind(Key, 1, Opts) of
    {_, Value} -> Value;
    _ -> Default
  end.

init(Ref, Socket, Transport, Opts) ->
    Env = [{listener, Ref} | get_value(env, Opts, [])],
    Middlewares = get_value(middlewares, Opts, [msgpack_rpc_handler]),
    MsgpackOpts = get_value(msgpack_opts, Opts, [jiffy]),
    Timeout = get_value(timeout, Opts, infinity),
    ok = ranch:accept_ack(Ref),
    middleware_init(#state{
        socket       = Socket,
        transport    = Transport,
        middlewares  = Middlewares,
        msgpack_opts = MsgpackOpts,
        env          = Env,
        timeout      = Timeout}).

middleware_init(State=#state{env=Env, middlewares=Middlewares}) ->
    middleware_init(State, Env, Middlewares).

middleware_init(State=#state{transport=Transport}, Env, []) ->
    State2 = loop_timeout(State),
    before_loop(State2#state{messages=Transport:messages()}, Env, <<>>);
middleware_init(State=#state{transport=Transport}, Env, [Middleware | Tail]) ->
    case Middleware:init({Transport:name(), msgpack_rpc}, Env) of
        {ok, Env2} ->
            middleware_init(State, Env2, Tail);
        {ok, Env2, hibernate} ->
            middleware_init(State#state{hibernate=true}, Env2, Tail);
        {ok, Env2, Timeout} ->
            middleware_init(State#state{timeout=Timeout}, Env2, Tail);
        {ok, Env2, Timeout, hibernate} ->
            middleware_init(State#state{hibernate=true, timeout=Timeout}, Env2, Tail);
        {shutdown, Env2} ->
            middleware_terminate(ok, Env2, State)
    end.

middleware_execute(State=#state{middlewares=Middlewares}, Env, Data, Callback, Req, NextState) ->
    middleware_execute(State, Env, Data, Callback, Req, NextState, Middlewares).

middleware_execute(State, Env, Data, _Callback, Req, NextState, []) ->
    Req:finalize(),
    State2 = loop_timeout(State),
    NextState(State2, Env, Data);
middleware_execute(State, Env, Data, Callback, Req, NextState, [Middleware | Tail]) ->
    case Middleware:Callback(Req, Env) of
        {ok, Req2, Env2} ->
            middleware_execute(State, Env2, Data, Callback, Req2, NextState, Tail);
        {ok, Req2, Env2, hibernate} ->
            middleware_execute(State#state{hibernate=true}, Env2, Data, Callback, Req2, NextState, Tail);
        {ok, Req2, Env2, Timeout} ->
            middleware_execute(State#state{timeout=Timeout}, Env2, Data, Callback, Req2, NextState, Tail);
        {ok, Req2, Env2, Timeout, hibernate} ->
            middleware_execute(State#state{hibernate=true, timeout=Timeout}, Env2, Data, Callback, Req2, NextState, Tail);
        {shutdown, Req2, Env2} ->
            middleware_terminate(ok, Req2, Env2, State)
    end.

middleware_info(State=#state{middlewares=Middlewares}, Env, Data, Callback, Info, NextState) ->
    middleware_info(State, Env, Data, Callback, Info, NextState, Middlewares).

middleware_info(State, Env, Data, _Callback, _Info, NextState, []) ->
    State2 = loop_timeout(State),
    NextState(State2, Env, Data);
middleware_info(State, Env, Data, Callback, Info, NextState, [Middleware | Tail]) ->
    case Middleware:Callback(Info, Env) of
        {ok, Env2} ->
            middleware_info(State, Env2, Data, Callback, Info, NextState, Tail);
        {ok, Env2, hibernate} ->
            middleware_info(State#state{hibernate=true}, Env2, Data, Callback, Info, NextState, Tail);
        {ok, Env2, Timeout} ->
            middleware_info(State#state{timeout=Timeout}, Env2, Data, Callback, Info, NextState, Tail);
        {ok, Env2, Timeout, hibernate} ->
            middleware_info(State#state{hibernate=true, timeout=Timeout}, Env2, Data, Callback, Info, NextState, Tail);
        {shutdown, Env2} ->
            middleware_terminate(ok, Env2, State)
    end.

middleware_terminate(Reason, Env, State=#state{middlewares=Middlewares}) ->
    middleware_terminate(Reason, undefined, Env, State, Middlewares).

middleware_terminate(Reason, Req, Env, State=#state{middlewares=Middlewares}) ->
    middleware_terminate(Reason, Req, Env, State, Middlewares).

middleware_terminate(Reason, _Req, _Env, State, []) ->
    terminate(Reason, State);
middleware_terminate(Reason, Req, Env, State, [Middleware | Tail]) ->
    Middleware:terminate(Reason, Req, Env),
    middleware_terminate(Reason, Req, Env, State, Tail).

loop_timeout(State=#state{timeout=infinity}) ->
    State#state{timeout_ref=undefined};
loop_timeout(State=#state{timeout=Timeout, timeout_ref=PrevRef}) ->
    _ = case PrevRef of
        undefined -> ignore;
        PrevRef -> erlang:cancel_timer(PrevRef)
    end,
    TRef = erlang:start_timer(Timeout, self(), ?MODULE),
    State#state{timeout_ref=TRef}.

before_loop(State=#state{hibernate=true, transport=Transport, socket=Socket}, Env, SoFar) ->
    Transport:setopts(Socket, [{active, once}]),
    erlang:hibernate(?MODULE, loop, [State#state{hibernate=false}, Env, SoFar]);
before_loop(State=#state{transport=Transport, socket=Socket}, Env, SoFar) ->
    Transport:setopts(Socket, [{active, once}]),
    loop(State, Env, SoFar).

loop(State=#state{socket=Socket, messages={OK, Closed, Error}, timeout_ref=TRef}, Env, SoFar) ->
    receive
        {OK, Socket, Data} ->
            State2 = loop_timeout(State),
            parse_data(State2, Env, << SoFar/binary, Data/binary >>);
        {Closed, Socket} ->
            middleware_terminate(State, Env, {error, closed});
        {Error, Socket, Reason} ->
            middleware_terminate(State, Env, {error, Reason});
        {timeout, TRef, ?MODULE} ->
            middleware_terminate(State, Env, {normal, timeout});
        {timeout, OlderTRef, ?MODULE} when is_reference(OlderTRef) ->
            loop(State, Env, SoFar);
        Message ->
            middleware_info(State, Env, SoFar, info, Message, fun loop/3)
    end.

parse_data(State=#state{transport=Transport, socket=Socket, msgpack_opts=MsgpackOpts}, Env, Data) ->
    case msgpack:unpack_stream(Data, MsgpackOpts) of
        {[?MSGPACK_RPC_REQUEST, MsgId, Method, Params], RemainingData} ->
            case msgpack_rpc_request:start_fsm(MsgId, Method, Params, Socket, Transport, MsgpackOpts) of
                {ok, Req} ->
                    % io:format("Request: ~p~n", [Req]),
                    middleware_execute(State, Env, RemainingData, request, Req, fun parse_data/3);
                {error, Reason} ->
                    middleware_terminate(Reason, Env, State)
            end;
        {[?MSGPACK_RPC_NOTIFY, Method, Params], RemainingData} ->
            case msgpack_rpc_notify:start_fsm(Method, Params) of
                {ok, Req} ->
                    % io:format("Notify: ~p~n", [Req]),
                    middleware_execute(State, Env, RemainingData, notify, Req, fun parse_data/3);
                {error, Reason} ->
                    middleware_terminate(Reason, Env, State)
            end;
        {error, incomplete} ->
            %% Need more data.
            before_loop(State, Env, Data);
        {error, _} = Error ->
            middleware_terminate(Error, Env, State)
    end.

terminate(_TerminateReason, _State=#state{transport=Transport, socket=Socket}) ->
    Transport:close(Socket),
    ok.

known_error_to_binary(undef) -> <<"undef">>;
known_error_to_binary(function_clause) -> <<"function_clause">>.

binary_to_known_error(<<"undef">>) -> undef;
binary_to_known_error(<<"function_clause">>) -> function_clause.
