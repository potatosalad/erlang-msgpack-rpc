%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  17 June 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------

-module(msgpack_rpc_client_fsm).
-behaviour(gen_fsm).

-include("msgpack_rpc.hrl").
-include("msgpack_rpc_client.hrl").

%% API
-export([start/1, start_link/1, shutdown/1]).
-export([get_stats/1, get_and_reset_stats/1,
         notify/2, request/2, join/2, request_join/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-export([connecting/2, connecting/3, ready/2, ready/3, idle/2, idle/3]).

%% Internal
-import(msgpack_rpc_util, [get_value/3]).

-type timestamp() :: {integer(), integer(), integer()}.

-record(call, {
    msg_id   = undefined    :: undefined | msgpack_rpc:msg_id(),
    request  = undefined    :: undefined | msgpack_rpc:request(),
    response = undefined    :: undefined | msgpack_rpc:response(),
    time     = erlang:now() :: timestamp(),
    waiting  = none         :: none | queue()
}).

-record(message, {
    event = undefined :: undefined |
        {notify, msgpack_rpc:method(), msgpack_rpc:params()} |
        {request, msgpack_rpc:method(), msgpack_rpc:params()} |
        {request_join, msgpack_rpc:method(), msgpack_rpc:params()},
    from = none :: none | {pid(), any()}
}).

-record(state, {
    %% Client
    client = undefined :: undefined | msgpack_rpc:client(),

    %% Connection
    socket   = undefined :: undefined | inet:socket(),
    messages = undefined :: undefined | {atom(), atom(), atom()},
    counter  = 0         :: msgpack_rpc:msg_id(),
    session  = []        :: [#call{}],
    buffer   = <<>>      :: binary(),

    %% Cache
    cache_queue = queue:new() :: queue(),
    cache_count = 0           :: non_neg_integer(),

    %% Reconnect
    reconnect_count = 0 :: non_neg_integer(),
    reconnect_wait  = 0 :: timeout(),

    %% Stats
    stat_count = dict:new() :: dict(),
    stat_time  = dict:new() :: dict(),

    %% Options
    options = #msgpack_rpc_options{} :: msgpack_rpc:options(),

    debug = false :: boolean(),
    stats = off   :: off | on | all,

    %% Cache Options
    cache        = false    :: boolean(),
    cache_blocks = true     :: boolean(),
    cache_capped = true     :: boolean(),
    cache_limit  = infinity :: infinity | non_neg_integer(),
    cache_stops  = false    :: boolean(),

    %% Reconnect Options
    reconnect       = true     :: boolean(),
    reconnect_limit = infinity :: infinity | non_neg_integer(),
    reconnect_min   = 100      :: timeout(),
    reconnect_max   = 5000     :: timeout(),

    %% Transport Options
    transport      = undefined :: undefined | module(),
    transport_opts = []        :: [proplists:property()]
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start(msgpack_rpc:client())
    -> {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(Client) ->
    msgpack_rpc_client_fsm_sup:start([Client]).

-spec start_link(msgpack_rpc:client())
    -> {ok, pid()} | ignore | {error, term()}.
start_link(Client) ->
    gen_fsm:start_link(?MODULE, [Client], []).

-spec shutdown(pid()) -> ok.
shutdown(Pid) ->
    gen_fsm:send_all_state_event(Pid, shutdown).

-spec get_stats(pid()) -> {ok, [{binary(), integer(), integer()}]}.
get_stats(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, get_stats).

-spec get_and_reset_stats(pid()) -> {ok, [{binary(), integer(), integer()}]}.
get_and_reset_stats(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, get_and_reset_stats).

-spec notify(pid(), {msgpack_rpc:method(), msgpack_rpc:params()})
    -> ok.
notify(Pid, {Method, Params}) ->
    gen_fsm:send_event(Pid, {notify, Method, Params}).

-spec request(pid(),
        {msgpack_rpc:method(), msgpack_rpc:params()} |
        {msgpack_rpc:method(), msgpack_rpc:params(), timeout()})
    -> {ok, msgpack_rpc:request()} | {error, term()}.
request(Pid, {Method, Params}) ->
    gen_fsm:sync_send_event(Pid, {request, Method, Params});
request(Pid, {Method, Params, Timeout}) ->
    gen_fsm:sync_send_event(Pid, {request, Method, Params}, Timeout).

-spec join(pid(),
        {msgpack_rpc:request()} |
        {msgpack_rpc:request(), timeout()})
    -> {ok, msgpack_rpc:response()}.
join(Pid, {Req}) ->
    gen_fsm:sync_send_event(Pid, {join, Req});
join(Pid, {Req, Timeout}) ->
    gen_fsm:sync_send_event(Pid, {join, Req}, Timeout).

-spec request_join(pid(),
        {msgpack_rpc:method(), msgpack_rpc:params()} |
        {msgpack_rpc:method(), msgpack_rpc:params(), timeout()})
    -> {ok, msgpack_rpc:request()} | {error, term()}.
request_join(Pid, {Method, Params}) ->
    gen_fsm:sync_send_event(Pid, {request_join, Method, Params});
request_join(Pid, {Method, Params, Timeout}) ->
    gen_fsm:sync_send_event(Pid, {request_join, Method, Params}, Timeout).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

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
init([Client]) ->
    Opts = msgpack_rpc_client:get(fsm_opts, Client),
    State = parse_options(Client, Opts),
    case try_connect(State) of
        {next_state, NextState, State2, Timeout} ->
            {ok, NextState, State2, Timeout}
    end.

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
handle_event(shutdown, _StateName, StateData) ->
    {stop, normal, StateData};

handle_event(Event, _StateName, StateData) ->
    {stop, {error, badevent, Event}, StateData}.

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
handle_sync_event(get_stats, _From, StateName, StateData) ->
    {reply, {ok, format_stats(StateData)}, StateName, StateData, 0};

handle_sync_event(get_and_reset_stats, _From, StateName, StateData) ->
    {reply, {ok, format_stats(StateData)}, StateName, reset_stats(StateData), 0};

handle_sync_event(Event, _From, _StateName, StateData) ->
    {stop, {error, badevent, Event}, StateData}.

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
handle_info({OK, Socket, Data}, StateName, State=#state{socket=Socket,
        messages={OK, _Closed, _Error}, buffer=SoFar}) ->
    parse_data(State, StateName, << SoFar/binary, Data/binary >>);

handle_info({Closed, Socket}, _StateName, State=#state{socket=Socket,
        messages={_OK, Closed, _Error}, reconnect=false}) ->
    {stop, {error, closed}, State};

handle_info({Closed, Socket}, _StateName, State=#state{socket=Socket,
        messages={_OK, Closed, _Error}, reconnect=true}) ->
    {next_state, connecting, State, 0};

handle_info({Error, Socket, Reason}, _StateName, State=#state{socket=Socket,
        messages={_OK, _Closed, Error}, reconnect=false}) ->
    {stop, {error, {Error, Reason}}, State};

handle_info({Error, Socket, _Reason}, _StateName, State=#state{socket=Socket,
        messages={_OK, _Closed, Error}, reconnect=true}) ->
    {next_state, connecting, State, 0};

handle_info(Info, _StateName, StateData) ->
    {stop, {error, badmsg, Info}, StateData}.

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
%% @spec connecting(Event :: term(), StateData :: term()) ->
%%     {next_state, NextStateName :: atom(), NewStateData :: term()} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {stop, Reason :: normal | shutdown | term(), NewStateData :: term()}
%% @end
%%--------------------------------------------------------------------
connecting(Event, State=#state{reconnect=false}) ->
    event_stop({reconnect, false}, Event, none, State);

connecting(Event, State=#state{reconnect_count=Limit,
        reconnect_limit=Limit}) ->
    event_stop({reconnect_limit, Limit}, Event, none, State);

connecting(timeout, State) ->
    reconnect(none, State);

connecting(#message{event=Event, from=none}, State) ->
    connecting(Event, State);

connecting(Message=#message{from=From}, State) when From =/= none ->
    Self = self(),
    spawn(fun() ->
        gen_fsm:sync_send_event(Self, Message)
    end),
    {next_state, connecting, State, 0};

connecting(Event, State=#state{cache=false}) ->
    event_failfast(Event, none, State);

connecting(Event, State=#state{cache=true, cache_count=Limit,
        cache_limit=Limit, cache_stops=true}) ->
    event_stop({failfast_limit, Limit}, Event, none, State);

connecting(Event, State=#state{cache=true, cache_count=Limit,
        cache_limit=Limit, cache_stops=false, cache_capped=false}) ->
    event_failfast(Event, none, State);

connecting(Event, State=#state{cache=true, cache_count=Limit,
        cache_limit=Limit, cache_stops=false, cache_capped=true}) ->
    event_enqueue_capped(Event, none, State);

connecting(Event, State=#state{cache=true}) ->
    event_enqueue(Event, none, State);

connecting(_Event, State) ->
    {next_state, connecting, State, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec connecting(Event :: term(), From :: {pid(), Tag :: term()}, StateData :: term()) ->
%%     {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
%%     {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term()} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
%%     {stop, Reason :: term(), NewStateData :: term()}
%% @end
%%--------------------------------------------------------------------
connecting(Event, From, State=#state{reconnect=false}) ->
    event_stop({reconnect, false}, Event, From, State);

connecting(Event, From, State=#state{reconnect_count=Limit,
        reconnect_limit=Limit}) ->
    event_stop({reconnect_limit, Limit}, Event, From, State);

connecting(Message=#message{from=none}, _From, State) ->
    gen_fsm:send_event(self(), Message),
    {reply, ok, connecting, State, 0};

connecting(#message{event=Event, from=OldFrom}, _From, State) when OldFrom =/= none ->
    connecting(Event, OldFrom, State);

connecting(Event, From, State=#state{cache=false}) ->
    event_failfast(Event, From, State);

connecting(Event = {join, _Req}, From, State) ->
    event_failfast(Event, From, State);

connecting(Event, From, State=#state{cache=true, cache_count=Limit,
        cache_limit=Limit, cache_stops=true}) ->
    event_stop({failfast_limit, Limit}, Event, From, State);

connecting(Event, From, State=#state{cache=true, cache_count=Limit,
        cache_limit=Limit, cache_stops=false, cache_capped=false}) ->
    event_failfast(Event, From, State);

connecting(Event, From, State=#state{cache=true, cache_count=Limit,
        cache_limit=Limit, cache_stops=false, cache_capped=true}) ->
    event_enqueue_capped(Event, From, State);

connecting(Event, From, State=#state{cache=true}) ->
    event_enqueue(Event, From, State);

connecting(_Event, _From, State) ->
    {next_state, connecting, State, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec ready(Event :: term(), StateData :: term()) ->
%%     {next_state, NextStateName :: atom(), NewStateData :: term()} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {stop, Reason :: normal | shutdown | term(), NewStateData :: term()}
%% @end
%%--------------------------------------------------------------------
ready(timeout, State=#state{cache_count=0}) ->
    {next_state, idle, State};
ready(timeout, State=#state{cache_count=Count, cache_queue=Queue}) ->
    case queue:out(Queue) of
        {{value, Message=#message{from=none}}, Queue2} ->
            gen_fsm:send_event(self(), Message),
            {next_state, ready, State#state{cache_count=Count-1, cache_queue=Queue2}, 0};
        {{value, Message=#message{from=From}}, Queue2} ->
            Self = self(),
            spawn(fun() ->
                Reply = gen_fsm:sync_send_event(Self, Message),
                gen_fsm:reply(From, Reply)
            end),
            {next_state, ready, State#state{cache_count=Count-1, cache_queue=Queue2}, 0};
        {empty, Queue} ->
            {next_state, idle, State#state{cache_count=0}}
    end;

ready(#message{event=Event, from=none}, State) ->
    idle(Event, State);

ready(Event, State=#state{cache_count=0}) ->
    idle(Event, State);

ready(Event, State=#state{cache=true, cache_count=Limit,
        cache_limit=Limit, cache_stops=false, cache_capped=false}) ->
    event_failfast(Event, none, State);

ready(Event, State=#state{cache=true, cache_blocks=true, cache_count=Limit,
        cache_limit=Limit, cache_stops=false, cache_capped=true}) ->
    event_enqueue_capped(Event, none, State);

ready(Event, State=#state{cache=true, cache_blocks=true}) ->
    event_enqueue(Event, none, State);

ready(Event, State) ->
    idle(Event, State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec ready(Event :: term(), From :: {pid(), Tag :: term()}, StateData :: term()) ->
%%     {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
%%     {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term()} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
%%     {stop, Reason :: term(), NewStateData :: term()}
%% @end
%%--------------------------------------------------------------------
ready(#message{event=Event, from=OldFrom}, From, State) when OldFrom =/= none ->
    idle(Event, From, State);

ready(Event, From, State=#state{cache_count=0}) ->
    idle(Event, From, State);

ready(Event, From, State=#state{cache=true, cache_count=Limit,
        cache_limit=Limit, cache_stops=false, cache_capped=false}) ->
    event_failfast(Event, From, State);

ready(Event, From, State=#state{cache=true, cache_blocks=true, cache_count=Limit,
        cache_limit=Limit, cache_stops=false, cache_capped=true}) ->
    event_enqueue_capped(Event, From, State);

ready(Event, From, State=#state{cache=true, cache_blocks=true}) ->
    event_enqueue(Event, From, State);

ready(Event, From, State) ->
    idle(Event, From, State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec idle(Event :: term(), StateData :: term()) ->
%%     {next_state, NextStateName :: atom(), NewStateData :: term()} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {stop, Reason :: normal | shutdown | term(), NewStateData :: term()}
%% @end
%%--------------------------------------------------------------------
idle(#message{event=Event, from=none}, State) ->
    idle(Event, State);

idle(#message{event=Event, from=From}, State) when From =/= none ->
    Self = self(),
    spawn(fun() ->
        Reply = gen_fsm:sync_send_event(Self, Event),
        gen_fsm:reply(From, Reply)
    end),
    {next_state, ready, State, 0};

idle(Event = {notify, _Method, _Params}, State) ->
    event_notify(Event, none, State);

idle(_Event, State) ->
    {next_state, ready, State, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec idle(Event :: term(), From :: {pid(), Tag :: term()}, StateData :: term()) ->
%%     {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
%%     {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term()} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
%%     {stop, Reason :: term(), NewStateData :: term()}
%% @end
%%--------------------------------------------------------------------
idle(#message{event=Event, from=none}, _From, State) ->
    gen_fsm:send_event(self(), Event),
    {reply, ok, ready, State, 0};

idle(#message{event=Event}, From, State) ->
    idle(Event, From, State);

idle({join, Req}, From, State) ->
    do_join(State, From, Req);

idle(Event = {request, _Method, _Params}, From, State) ->
    event_request(Event, From, State);

idle(Event = {request_join, _Method, _Params}, From, State) ->
    event_request_join(Event, From, State);

idle(Event, _From, State) ->
    {reply, {error, {badevent, Event}}, ready, State, 0}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% Event handlers

%% @private
event_enqueue(Event, From, State=#state{cache_count=Count, cache_queue=Queue}) ->
    Message = #message{event=Event, from=From},
    State2 = State#state{cache_count=Count+1, cache_queue=queue:in(Message, Queue), reconnect_wait=0},
    State3 = report(Event, From, State2, [{queue, 1}]),
    reconnect(none, State3).

%% @private
event_enqueue_capped(Event, From, State=#state{cache_queue=Queue0}) ->
    Queue = case queue:out(Queue0) of
        {{value, #message{from=OldFrom}}, Queue2} when OldFrom =/= none ->
            gen_fsm:reply(OldFrom, {error, dropped}),
            Queue2;
        {{value, _}, Queue2} ->
            Queue2;
        {empty, Queue0} ->
            Queue0
    end,
    Message = #message{event=Event, from=From},
    State2 = State#state{cache_queue=queue:in(Message, Queue), reconnect_wait=0},
    State3 = report(Event, From, State2, [{queue, 1}, {dropped, 1}]),
    reconnect(none, State3).

%% @private
event_failfast(Event, From, State) ->
    State2 = report(Event, From, State, [{failfast, 1}]),
    reconnect(From, State2).

%% @private
event_notify(Event = {notify, Method, Params}, none, State) ->
    Start = erlang:now(),
    Msg = #message{event=Event},
    Obj = msgpack_rpc_notify:new(Method, Params),

    try do_notify(State, Obj) of
        {ok, State2} ->
            Time = timer:now_diff(erlang:now(), Start),
            State3 = report(Event, none, State2, [{send, Time}]),
            {next_state, ready, State3, 0}
    catch
        Class:Reason ->
            error_logger:warning_msg(
                "** ~p ~p non-fatal error in ~p/~p~n"
                "   for the reason ~p:~p~n** Message was ~p~n"
                "** Stacktrace: ~p~n~n",
                [?MODULE, self(), event_notify, 3, Class, Reason, Msg, erlang:get_stacktrace()]),

            Time = timer:now_diff(erlang:now(), Start),
            case State#state.cache of
                false ->
                    State2 = report(Event, none, State, [{error, Time}, {failfast, 1}]),
                    {next_state, connecting, State2, 0};
                true ->
                    Count = State#state.cache_count + 1,
                    Queue = queue:in(Msg, State#state.cache_queue),
                    State2 = State#state{cache_count=Count, cache_queue=Queue},
                    State3 = report(Event, none, State2, [{error, Time}, {failfast, 1}]),
                    {next_state, connecting, State3, 0}
            end
    end.

%% @private
event_request(Event = {request, Method, Params}, From, State0=#state{counter=MsgId}) ->
    State = State0#state{counter=MsgId+1},
    Start = erlang:now(),
    Msg = #message{event=Event, from=From},
    Obj = msgpack_rpc_request:new(MsgId, Method, Params),

    try do_request(State, Obj) of
        {ok, State2} ->
            Time = timer:now_diff(erlang:now(), Start),
            State3 = report(Event, From, State2, [{send, Time}]),
            {reply, {ok, Obj}, ready, State3, 0}
    catch
        Class:Reason ->
            error_logger:warning_msg(
                "** ~p client ~p non-fatal error in ~p/~p~n"
                "   for the reason ~p:~p~n** Message was ~p~n"
                "** Stacktrace: ~p~n~n",
                [?MODULE, self(), event_request, 3, Class, Reason, Msg, erlang:get_stacktrace()]),

            Time = timer:now_diff(erlang:now(), Start),
            case State#state.cache of
                false ->
                    State2 = report(Event, From, State, [{error, Time}, {failfast, 1}]),
                    {next_state, connecting, State2, 0};
                true ->
                    Count = State#state.cache_count + 1,
                    Queue = queue:in(Msg, State#state.cache_queue),
                    State2 = State#state{cache_count=Count, cache_queue=Queue},
                    State3 = report(Event, From, State2, [{error, Time}, {failfast, 1}]),
                    {next_state, connecting, State3, 0}
            end
    end.

%% @private
event_request_join(Event = {request_join, Method, Params}, From, State0=#state{counter=MsgId}) ->
    State = State0#state{counter=MsgId+1},
    Start = erlang:now(),
    Msg = #message{event=Event, from=From},
    Obj = msgpack_rpc_request:new(MsgId, Method, Params),

    try do_request(State, Obj) of
        {ok, State2} ->
            Time = timer:now_diff(erlang:now(), Start),
            State3 = report(Event, From, State2, [{send, Time}]),
            do_join(State3, From, Obj)
    catch
        Class:Reason ->
            error_logger:warning_msg(
                "** ~p client ~p non-fatal error in ~p/~p~n"
                "   for the reason ~p:~p~n** Message was ~p~n"
                "** Stacktrace: ~p~n~n",
                [?MODULE, self(), event_request_join, 3, Class, Reason, Msg, erlang:get_stacktrace()]),

            Time = timer:now_diff(erlang:now(), Start),
            case State#state.cache of
                false ->
                    State2 = report(Event, From, State, [{error, Time}, {failfast, 1}]),
                    {next_state, connecting, State2, 0};
                true ->
                    Count = State#state.cache_count + 1,
                    Queue = queue:in(Msg, State#state.cache_queue),
                    State2 = State#state{cache_count=Count, cache_queue=Queue},
                    State3 = report(Event, From, State2, [{error, Time}, {failfast, 1}]),
                    {next_state, connecting, State3, 0}
            end
    end.

%% @private
event_stop(Reason, _Event, none, State) ->
    {stop, {error, Reason}, State};
event_stop(Reason, _Event, _From, State) ->
    {stop, {error, Reason}, {error, Reason}, State}.

%% @private
do_join(State=#state{session=Session}, From, Obj) ->
    MsgId = msgpack_rpc_request:get(msg_id, Obj),
    case lists:keytake(MsgId, #call.msg_id, Session) of
        false ->
            {reply, {error, request_not_found}, ready, State, 0};
        {value, Call=#call{msg_id=MsgId, waiting=Waiting, response=undefined}, Session2} ->
            Queue = case Waiting of
                none -> queue:new();
                _    -> Waiting
            end,
            Call2 = Call#call{waiting=queue:in(From, Queue)},
            {next_state, ready, State#state{session=[Call2 | Session2]}, 0};
        {value, #call{msg_id=MsgId, response=Response}, Session2} ->
            {reply, {ok, Response}, ready, State#state{session=Session2}, 0}
    end.

%% @private
do_notify(State=#state{socket=Socket, transport=Transport,
        options=#msgpack_rpc_options{msgpack_packer=Packer}}, Obj) ->
    Object = msgpack_rpc_notify:to_msgpack_object(Obj),
    case Packer(Object) of
        {error, PackerReason} ->
            erlang:error(PackerReason);
        Packet ->
            case Transport:send(Socket, Packet) of
                ok ->
                    {ok, State};
                {error, SocketReason} ->
                    erlang:error(SocketReason)
            end
    end.

%% @private
do_request(State=#state{socket=Socket, transport=Transport, session=Session,
        options=#msgpack_rpc_options{msgpack_packer=Packer}}, Obj) ->
    Object = msgpack_rpc_request:to_msgpack_object(Obj),
    case Packer(Object) of
        {error, PackerReason} ->
            erlang:error(PackerReason);
        Packet ->
            case Transport:send(Socket, Packet) of
                ok ->
                    MsgId = msgpack_rpc_request:get(msg_id, Obj),
                    Call = #call{msg_id=MsgId, request=Obj},
                    {ok, State#state{session=[Call | Session]}};
                {error, SocketReason} ->
                    erlang:error(SocketReason)
            end
    end.

%% @private
do_response(State=#state{session=Session, options=#msgpack_rpc_options{error_decoder=ErrorDecoder}},
        StateName, Data, {MsgId, Error, Result}) ->
    case lists:keytake(MsgId, #call.msg_id, Session) of
        false ->
            parse_data(State, StateName, Data);
        {value, Call=#call{msg_id=MsgId, request=Req, time=Start, waiting=Waiting}, Session2} ->
            Time = timer:now_diff(erlang:now(), Start),
            Method = msgpack_rpc_request:get(method, Req),
            Response = msgpack_rpc_response:new(Req, MsgId, ErrorDecoder(Error), Result),
            State2 = case Waiting of
                none ->
                    Call2 = Call#call{response=Response, time=Time},
                    State#state{session=[Call2 | Session2]};
                _ ->
                    respond(Response, Waiting),
                    State#state{session=Session2}
            end,
            parse_data(inc_stats(Method, [request, response], Time, State2), StateName, Data)
    end.

%% @private
parse_data(State=#state{socket=Socket, transport=Transport,
        options=#msgpack_rpc_options{msgpack_unpacker=Unpacker}}, StateName, Data) ->
    case Unpacker(Data) of
        {[?MSGPACK_RPC_RESPONSE, MsgId, Error, Result], RemainingData} ->
            do_response(State, StateName, RemainingData, {MsgId, Error, Result});
        {error, incomplete} ->
            %% Need more data.
            ok = Transport:setopts(Socket, [{active, once}]),
            {next_state, StateName, State#state{buffer=Data}, 0};
        {error, {badarg, Reason}} ->
            {stop, {error, Reason}, State#state{buffer=Data}};
        {error, _Reason} ->
            ok = Transport:setopts(Socket, [{active, once}]),
            {next_state, StateName, State#state{buffer=Data}, 0}
    end.

%% @private
parse_options(Client, OrigOpts) ->
    {FsmOpts, Opts} = msgpack_rpc_util:partition_options(OrigOpts, [
        debug, stats, cache, cache_blocks, cache_capped, cache_limit,
        cache_stops, reconnect, reconnect_limit, reconnect_min,
        reconnect_max, transport, transport_options]),

    State = #state{client=Client},

    Options = msgpack_rpc_options:new(Opts),
    Debug = get_value(debug, FsmOpts, State#state.debug),
    Stats = get_value(stats, FsmOpts, State#state.stats),

    %% Cache Options
    Cache = get_value(cache, FsmOpts, State#state.cache),
    CacheBlocks = get_value(cache_blocks, FsmOpts, State#state.cache_blocks),
    CacheCapped = get_value(cache_capped, FsmOpts, State#state.cache_capped),
    CacheLimit = get_value(cache_limit, FsmOpts, State#state.cache_limit),
    CacheStops = get_value(cache_stops, FsmOpts, State#state.cache_stops),

    %% Reconnect Options
    Reconnect = get_value(reconnect, FsmOpts, State#state.reconnect),
    ReconnectLimit = get_value(reconnect_limit, FsmOpts, State#state.reconnect_limit),
    ReconnectMin = get_value(reconnect_min, FsmOpts, State#state.reconnect_min),
    ReconnectMax = get_value(reconnect_max, FsmOpts, State#state.reconnect_max),

    %% Transport Options
    Transport = msgpack_rpc_util:known_transport(get_value(transport, FsmOpts, ranch_tcp)),
    TransportOpts = get_value(transport_options, FsmOpts, []),

    State#state{messages=Transport:messages(), options=Options,
        debug=Debug, stats=Stats, cache=Cache, cache_blocks=CacheBlocks,
        cache_capped=CacheCapped, cache_limit=CacheLimit,
        cache_stops=CacheStops, reconnect=Reconnect,
        reconnect_limit=ReconnectLimit, reconnect_min=ReconnectMin,
        reconnect_max=ReconnectMax, transport=Transport,
        transport_opts=TransportOpts}.

%% @private
reconnect(none, State) ->
    try_connect(State);
reconnect(From, State) when From =/= none ->
    case try_connect(State) of
        {next_state, NextState, State2, Timeout} ->
            {reply, {error, no_connection}, NextState, State2, Timeout}
    end.

%% @private
reconnect_wait(#state{reconnect_min=Min, reconnect_wait=0}) ->
    Min;
reconnect_wait(#state{reconnect_max=Max, reconnect_wait=Max}) ->
    Max;
reconnect_wait(#state{reconnect_max=Max, reconnect_wait=R}) ->
    Backoff = 2 * R,
    case Backoff > Max of
        true  -> Max;
        false -> Backoff
    end.

%% @private
respond(Response, Queue) ->
    case queue:out(Queue) of
        {{value, From}, Queue2} ->
            gen_fsm:reply(From, {ok, Response}),
            respond(Response, Queue2);
        {empty, Queue} ->
            ok
    end.

%% @private
try_connect(State=#state{socket=OldSocket, transport=Transport,
        transport_opts=TransportOpts, reconnect_count=Count,
        client=#msgpack_rpc_client{address=Address, port=Port}}) ->
    case OldSocket of
        undefined -> ok;
        _         -> (catch Transport:close(OldSocket))
    end,
    case Transport:connect(Address, Port, TransportOpts) of
        {ok, Socket} ->
            Transport:controlling_process(Socket, self()),
            ok = Transport:setopts(Socket, [{active, once}]),
            {next_state, ready, State#state{socket=Socket, reconnect_count=0, reconnect_wait=0}, 0};
        {E, Reason} when E == error; E == exception ->
            Wait = reconnect_wait(State),
            case State#state.debug of
                true ->
                    error_logger:warning_msg("[~w] ~w connect failed (~w), trying again in ~w ms~n",
                        [self(), ?MODULE, Reason, Wait]);
                _ ->
                    ok
            end,
            State2 = State#state{socket=undefined, reconnect_count=Count+1, reconnect_wait=Wait},
            {next_state, connecting, State2, Wait}
    end.

%% Stats

%% @private
format_stats(#state{stat_count=StatCount, stat_time=StatTime}) ->
    F = fun(Key, Count, Stats) ->
        Micros = dict:fetch(Key, StatTime),
        [{Key, Count, Micros} | Stats]
    end,
    dict:fold(F, [], StatCount).

%% @private
inc_stats(_Method, _Op, _Time, State=#state{stats=off}) ->
    State;
inc_stats(_Method, Op, Time, State=#state{stats=on}) ->
    inc_stats(undefined, Op, Time, State);
inc_stats(Method, Op, Time, State=#state{stats=all}) when is_atom(Method), Method =/= undefined ->
    inc_stats(atom_to_binary(Method, utf8), Op, Time, State);
inc_stats(Method, Op, Time, State=#state{stats=all}) when is_list(Method) ->
    inc_stats(list_to_binary(Method), Op, Time, State);
inc_stats(Method, Op, Time, State) when is_atom(Op) ->
    inc_stats(Method, atom_to_binary(Op, utf8), Time, State);
inc_stats(Method, Op = [Head | Tail], Time, State) when is_list(Op), is_atom(Head) ->
    inc_stats(Method, << (atom_to_binary(Head, utf8))/binary, << <<"_", (atom_to_binary(T, utf8))/binary>> || T <- Tail >>/binary >>, Time, State);
inc_stats(Method, Op, Time, State=#state{stats=all}) ->
    Key = <<Method/binary, "_", Op/binary>>,
    increment_stat(Op, Time, increment_stat(Key, Time, State));
inc_stats(_Method, Op, Time, State=#state{stats=on}) ->
    Key = Op,
    increment_stat(Key, Time, State).

%% @private
increment_stat(Stat, Time, State=#state{stat_count=StatCount, stat_time=StatTime}) ->
    StatCount2 = dict:update_counter(Stat, 1, StatCount),
    StatTime2 = dict:update_counter(Stat, Time, StatTime),
    State#state{stat_count=StatCount2, stat_time=StatTime2}.

%% @private
reset_stats(State) ->
    State#state{stat_count=dict:new(), stat_time=dict:new()}.

%% @private
report(_Event, _From, State, []) ->
    State;
report(Event = {notify, Method, _Params}, none, State, [{Op, Val} | Stats]) ->
    State2 = inc_stats(Method, [notify, Op], Val, State),
    report(Event, none, State2, Stats);
report(Event = {request, Method, _Params}, From, State, [{Op, Val} | Stats]) ->
    State2 = inc_stats(Method, [request, Op], Val, State),
    report(Event, From, State2, Stats);
report(Event = {request_join, Method, _Params}, From, State, [{Op, Val} | Stats]) ->
    State2 = inc_stats(Method, [request, Op], Val, State),
    report(Event, From, State2, Stats);
report(Event, From, State, Stats) ->
    io:format("UNKNOWN REPORT: ~p ~p ~p ~p~n", [Event, From, State, Stats]),
    State.
