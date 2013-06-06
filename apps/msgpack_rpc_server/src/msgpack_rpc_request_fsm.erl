%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  31 May 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------

-module(msgpack_rpc_request_fsm).
-behaviour(gen_fsm).

%% API
-export([
    start_link/1,
    start/6,
    execute/2,
    result/2,
    error/2,
    response/3,
    finalize/1
]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-export([waiting/2, waiting/3, executing/2, executing/3,
         responding/2, responding/3, finalized/2, finalized/3]).

-record(state, {
    req     = undefined :: undefined | msgpack_rpc_request:req(),
    task    = undefined :: undefined | function() | {function(), [any()]} | {module(), atom(), [any()]},
    error   = nil       :: nil | any(),
    result  = nil       :: nil | any()
}).

start_link(Req) ->
    gen_fsm:start_link(?MODULE, [Req], []).

start(MsgId, Method, Params, Socket, Transport, MsgpackOpts) ->
    Req = msgpack_rpc_request:new(MsgId, Method, Params, Socket, Transport, MsgpackOpts),
    case msgpack_rpc_request_fsm_sup:start([Req]) of
        {ok, FsmRef} ->
            {ok, msgpack_rpc_request:set([{fsm_ref, FsmRef}], Req)};
        {ok, FsmRef, _} ->
            {ok, msgpack_rpc_request:set([{fsm_ref, FsmRef}], Req)};
        Other ->
            Other
    end.

execute(Task, FsmRef) ->
    gen_fsm:send_event(FsmRef, {execute, Task}).

result(Result, FsmRef) ->
    gen_fsm:send_event(FsmRef, {result, Result}).

error(Error, FsmRef) ->
    gen_fsm:send_event(FsmRef, {error, Error}).

response(Error, Result, FsmRef) ->
    gen_fsm:send_event(FsmRef, {response, Error, Result}).

finalize(FsmRef) ->
    gen_fsm:send_all_state_event(FsmRef, finalize).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec init(Args :: term()) ->
%%     {ok, StateName :: atom(), StateData :: term()} |
%%     {ok, StateName :: atom(), StateData :: term(), timeout() | hibernate} |
%%     {stop, Reason :: term()} | ignore
%% @end
%%--------------------------------------------------------------------
init([Request]) ->
    State = #state{req=Request},
    {ok, waiting, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec handle_event(Event :: term(), StateName :: atom(),
%%                    StateData :: term()) ->
%%     {next_state, NextStateName :: atom(), NewStateData :: term()} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {stop, Reason :: term(), NewStateData :: term()}
%% @end
%%--------------------------------------------------------------------
handle_event(finalize, finalized, State) ->
    {stop, normal, State};
handle_event(finalize, StateName, State) ->
    {next_state, StateName, State, 0};
handle_event(_Event, _StateName, StateData) ->
    {stop, badmsg, StateData}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()},
%%                         StateName :: atom(), StateData :: term()) ->
%%     {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
%%     {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term()} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
%%     {stop, Reason :: term(), NewStateData :: term()}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop, badmsg, StateData}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec handle_info(Info :: term(), StateName :: atom(),
%%                   StateData :: term()) ->
%%     {next_state, NextStateName :: atom(), NewStateData :: term()} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {stop, Reason :: normal | term(), NewStateData :: term()}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, _StateName, StateData) ->
    {stop, badmsg, StateData}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
%%                 StateName :: atom(), StateData :: term()) ->
%%     term()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _StateData) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec code_change(_OldVsn :: term() | {down, term()}, StateName :: atom(),
%%                   StateData :: term(), _Extra :: term()) ->
%%     {ok, NextStateName :: atom(), NewStateData :: term()}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec waiting(Event :: term(), StateData :: term()) ->
%%     {next_state, NextStateName :: atom(), NewStateData :: term()} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {stop, Reason :: normal | shutdown | term(), NewStateData :: term()}
%% @end
%%--------------------------------------------------------------------
waiting({execute, Task}, State) ->
    {next_state, executing, State#state{task=Task}, 0};
waiting(_Event, State) ->
    {next_state, waiting, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec waiting(Event :: term(), From :: {pid(), Tag :: term()}, StateData :: term()) ->
%%     {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
%%     {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term()} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
%%     {stop, Reason :: term(), NewStateData :: term()}
%% @end
%%--------------------------------------------------------------------
waiting(_Event, _From, State) ->
    {next_state, waiting, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec executing(Event :: term(), StateData :: term()) ->
%%     {next_state, NextStateName :: atom(), NewStateData :: term()} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {stop, Reason :: normal | shutdown | term(), NewStateData :: term()}
%% @end
%%--------------------------------------------------------------------
executing(timeout, State=#state{req=Req, task=Task}) ->
    try run(Task) of
        {ok, _Req2} ->
            {next_state, responding, State};
        {result, Result, _Req2} ->
            {next_state, responding, State#state{result=Result}, 0};
        {error, Error, _Req2} ->
            {next_state, responding, State#state{error=Error}, 0};
        {response, Error, Result, _Req2} ->
            {next_state, responding, State#state{error=Error, result=Result}, 0}
    catch Class:Reason ->
        error_logger:warning_msg(
            "** ~p task ~p non-fatal error in ~p/~p~n"
            "   for the reason ~p:~p~n** Request was ~p~n"
            "** State was ~p~n** Stacktrace: ~p~n~n",
            [?MODULE, Task, executing, 2, Class, Reason, Req, State, erlang:get_stacktrace()]),
        {next_state, responding, State#state{error=Reason}, 0}
    end;
executing(_Event, State) ->
    {next_state, executing, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec executing(Event :: term(), From :: {pid(), Tag :: term()}, StateData :: term()) ->
%%     {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
%%     {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term()} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
%%     {stop, Reason :: term(), NewStateData :: term()}
%% @end
%%--------------------------------------------------------------------
executing(_Event, _From, State) ->
    {next_state, executing, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec responding(Event :: term(), StateData :: term()) ->
%%     {next_state, NextStateName :: atom(), NewStateData :: term()} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {stop, Reason :: normal | shutdown | term(), NewStateData :: term()}
%% @end
%%--------------------------------------------------------------------
responding(timeout, State=#state{error=Error, result=Result, req=Req}) ->
    msgpack_rpc_request:reply(Error, Result, Req),
    {next_state, finalized, State, 0};
responding({response, Error, Result}, State=#state{req=Req}) ->
    msgpack_rpc_request:reply(Error, Result, Req),
    {next_state, finalized, State#state{error=Error, result=Result}, 0};
responding({result, Result}, State=#state{error=Error, req=Req}) ->
    msgpack_rpc_request:reply(Error, Result, Req),
    {next_state, finalized, State#state{result=Result}, 0};
responding({error, Error}, State=#state{result=Result, req=Req}) ->
    msgpack_rpc_request:reply(Error, Result, Req),
    {next_state, finalized, State#state{error=Error}, 0};
responding(_Event, State) ->
    {next_state, responding, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec responding(Event :: term(), From :: {pid(), Tag :: term()}, StateData :: term()) ->
%%     {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
%%     {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term()} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
%%     {stop, Reason :: term(), NewStateData :: term()}
%% @end
%%--------------------------------------------------------------------
responding(_Event, _From, State) ->
    {next_state, responding, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec finalized(Event :: term(), StateData :: term()) ->
%%     {next_state, NextStateName :: atom(), NewStateData :: term()} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {stop, Reason :: normal | shutdown | term(), NewStateData :: term()}
%% @end
%%--------------------------------------------------------------------
finalized(timeout, State) ->
    {stop, normal, State};
finalized(_Event, State) ->
    {next_state, finalized, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec finalized(Event :: term(), From :: {pid(), Tag :: term()}, StateData :: term()) ->
%%     {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
%%     {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term()} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
%%     {stop, Reason :: term(), NewStateData :: term()}
%% @end
%%--------------------------------------------------------------------
finalized(_Event, _From, State) ->
    {next_state, finalized, State}.

run({Module, Function, Args}) ->
    Module:Function(Args);
run({Task, Args}) when is_function(Task) ->
    Task(Args);
run(Task) when is_function(Task) ->
    Task().
