%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  31 May 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------

-module(msgpack_rpc_task_fsm).
-behaviour(gen_fsm).

-include("msgpack_rpc.hrl").
-include("msgpack_rpc_server.hrl").

%% API
-export([start/1,
         start_link/1,
         execute/2,
         respond/2,
         shutdown/1,
         sync_execute/2,
         sync_respond/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-export([waiting/2, waiting/3, executing/2, executing/3,
         responding/2, responding/3, finalized/2, finalized/3]).

%% Internal functions
-export([job_runner/3]).

-type job() :: function() | {function(), [any()]} | {module(), atom(), [any()]}.
-export_type([job/0]).

-record(state, {
    task    = undefined :: undefined | msgpack_rpc:task(),
    job     = undefined :: undefined | job(),
    job_pid = undefined :: undefined | pid(),
    job_ref = undefined :: undefined | reference(),
    queue   = undefined :: undefined | queue()
}).

start(Task) ->
    msgpack_rpc_task_fsm_sup:start([Task]).

start_link(Task) ->
    gen_fsm:start_link(?MODULE, [Task], []).

execute(Pid, Job) ->
    gen_fsm:send_event(Pid, {execute, Job}).

respond(Pid, Resp) ->
    gen_fsm:send_event(Pid, {respond, Resp}).

shutdown(Pid) ->
    gen_fsm:send_event(Pid, shutdown).

sync_execute(Pid, Job) ->
    gen_fsm:sync_send_event(Pid, {execute, Job}).

sync_respond(Pid, Resp) ->
    gen_fsm:sync_send_event(Pid, {respond, Resp}).

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
init([Task]) ->
    State = #state{task=Task, queue=queue:new()},
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
handle_info({'DOWN', JobRef, process, JobPid, normal}, StateName, StateData=#state{job_pid=JobPid, job_ref=JobRef}) ->
    {next_state, StateName, StateData, 0};
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
waiting({execute, Job}, State) ->
    State2 = State#state{job=Job},
    {JobPid, JobRef} = spawn_job(Job, State2),
    {next_state, executing, State2#state{job_pid=JobPid, job_ref=JobRef}};
waiting({respond, Resp}, State=#state{task=#msgpack_rpc_task{type=request}}) ->
    State2 = prepare_response(Resp, State),
    {next_state, responding, State2, 0};
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
waiting({execute, Job}, From, State=#state{queue=Queue}) ->
    State2 = State#state{job=Job, queue=queue:in(From, Queue)},
    {JobPid, JobRef} = spawn_job(Job, State2),
    {next_state, executing, State2#state{job_pid=JobPid, job_ref=JobRef}};
waiting({respond, Resp}, From, State=#state{task=#msgpack_rpc_task{type=request}}) ->
    State2 = prepare_response(Resp, State),
    Queue = State2#state.queue,
    {next_state, responding, State2#state{queue=queue:in(From, Queue)}, 0};
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
executing(shutdown, State=#state{task=#msgpack_rpc_task{type=notify}}) ->
    State2 = process_queue(ok, State),
    {next_state, finalized, State2, 0};
executing({respond, Resp}, State=#state{task=#msgpack_rpc_task{type=request}}) ->
    State2 = prepare_response(Resp, State),
    {next_state, responding, State2, 0};
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
executing(shutdown, _From, State=#state{task=#msgpack_rpc_task{type=notify}}) ->
    State2 = process_queue(ok, State),
    {reply, ok, finalized, State2, 0};
executing({respond, Resp}, From, State=#state{task=#msgpack_rpc_task{type=request}}) ->
    State2 = prepare_response(Resp, State),
    Queue = State2#state.queue,
    {next_state, responding, State2#state{queue=queue:in(From, Queue)}, 0};
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
responding(timeout, State) ->
    State2 = process_response(State),
    {next_state, finalized, State2, 0};
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

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

job_runner(#state{task=#msgpack_rpc_task{message=Message, type=notify}}, Job, Pid) ->
    try
        exec_job(Job)
    catch
        Class:Reason ->
            error_logger:error_msg(
                "** ~p job ~p terminating in ~p/~p~n"
                "   for the reason ~p:~p~n** Message was ~p~n"
                "** Stacktrace: ~p~n~n",
                [?MODULE, Job, job_runner, 3, Class, Reason, Message, erlang:get_stacktrace()])
    after
        shutdown(Pid)
    end;
job_runner(#state{task=#msgpack_rpc_task{message=Message, type=request}}, Job, Pid) ->
    try exec_job(Job) of
        {respond, Resp} ->
            respond(Pid, Resp);
        _ ->
            ok
    catch
        Class:Reason ->
            error_logger:error_msg(
                "** ~p job ~p terminating in ~p/~p~n"
                "   for the reason ~p:~p~n** Message was ~p~n"
                "** Stacktrace: ~p~n~n",
                [?MODULE, Job, job_runner, 3, Class, Reason, Message, erlang:get_stacktrace()]),
            respond(Pid, {error, Reason})
    end.

prepare_response({error, Error}, State=#state{task=Task=#msgpack_rpc_task{type=request, message=Message,
        options=#msgpack_rpc_options{error_encoder=ErrorEncoder}}}) ->
    MsgId = msgpack_rpc_request:get(msg_id, Message),
    Response = msgpack_rpc_response:new(Message, MsgId, ErrorEncoder(Error), nil),
    process_queue(Response, State#state{task=Task#msgpack_rpc_task{response=Response}});
prepare_response({result, Result}, State=#state{task=Task=#msgpack_rpc_task{type=request, message=Message,
        options=#msgpack_rpc_options{error_encoder=ErrorEncoder}}}) ->
    MsgId = msgpack_rpc_request:get(msg_id, Message),
    Response = msgpack_rpc_response:new(Message, MsgId, ErrorEncoder(nil), Result),
    process_queue(Response, State#state{task=Task#msgpack_rpc_task{response=Response}}).

process_response(State=#state{task=#msgpack_rpc_task{response=Response, socket=Socket,
        transport=Transport, options=#msgpack_rpc_options{msgpack_packer=Packer}}}) ->
    Object = msgpack_rpc_response:to_msgpack_object(Response),
    Packet = Packer(Object),
    Transport:send(Socket, Packet),
    process_queue(Response, State).

process_queue(Reply, State=#state{queue=Queue}) ->
    case queue:out(Queue) of
        {{value, From}, Queue2} ->
            gen_fsm:reply(From, Reply),
            process_queue(Reply, State#state{queue=Queue2});
        {empty, Queue} ->
            State
    end.

exec_job({Module, Function, Args}) ->
    Module:Function(Args);
exec_job({Job, Args}) when is_function(Job) ->
    Job(Args);
exec_job(Job) when is_function(Job) ->
    Job().

spawn_job(Job, State) ->
    erlang:spawn_monitor(?MODULE, job_runner, [State, Job, self()]).
