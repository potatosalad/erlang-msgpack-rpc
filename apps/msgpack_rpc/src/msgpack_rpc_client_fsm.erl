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
         join/2, join/3,
         notify/3,
         request/3, request/4]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-export([connecting/2, connecting/3, ready/2, ready/3]).

-type timestamp() :: {integer(), integer(), integer()}.

-record(call, {
    msg_id   = undefined    :: undefined | msgpack_rpc:msg_id(),
    request  = undefined    :: undefined | msgpack_rpc:request(),
    response = undefined    :: undefined | msgpack_rpc:response(),
    time     = erlang:now() :: timestamp(),
    waiting  = none         :: none | queue()
}).

-type call() :: #call{}.

-record(state, {
    %% Client
    client = undefined :: undefined | msgpack_rpc:client(),

    %% Connection
    socket   = undefined :: undefined | inet:socket(),
    messages = undefined :: undefined | {atom(), atom(), atom()},
    counter  = 0         :: msgpack_rpc:msg_id(),
    session  = []        :: [call()],
    buffer   = <<>>      :: binary(),

    %% Reconnection
    failfast_queue  = queue:new() :: undefined | queue(),
    failfast_count  = 0           :: non_neg_integer(),
    reconnect_count = 0           :: non_neg_integer(),
    reconnect_time  = 0           :: timeout(),

    %% Stats
    stat_count = dict:new() :: dict(),
    stat_time  = dict:new() :: dict(),

    %% Options
    options = #msgpack_rpc_options{} :: msgpack_rpc:options(),

    debug           = false     :: boolean(),
    failfast        = true      :: boolean(),
    failfast_limit  = none      :: none | non_neg_integer(),
    reconnect       = true      :: boolean(),
    reconnect_limit = none      :: none | non_neg_integer(),
    reconnect_min   = 100       :: timeout(),
    reconnect_max   = 5000      :: timeout(),
    stats           = off       :: off | on | all,
    transport       = undefined :: undefined | atom(),
    transport_opts  = []        :: [proplists:property()]
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

-spec join(pid(), msgpack_rpc:request()) -> {ok, msgpack_rpc:response()}.
join(Pid, Req) ->
    gen_fsm:sync_send_event(Pid, {join, Req}).

-spec join(pid(), msgpack_rpc:request(), timeout()) -> {ok, msgpack_rpc:response()}.
join(Pid, Req, Timeout) ->
    gen_fsm:sync_send_event(Pid, {join, Req}, Timeout).

-spec notify(pid(), msgpack_rpc:method(), msgpack_rpc:params()) -> ok.
notify(Pid, Method, Params) ->
    gen_fsm:send_event(Pid, {notify, Method, Params}).

-spec request(pid(), msgpack_rpc:method(), msgpack_rpc:params())
    -> {ok, msgpack_rpc:request()} | {error, term()}.
request(Pid, Method, Params) ->
    gen_fsm:sync_send_event(Pid, {request, Method, Params}).

-spec request(pid(), msgpack_rpc:method(), msgpack_rpc:params(), timeout())
    -> {ok, msgpack_rpc:request()} | {error, term()}.
request(Pid, Method, Params, Timeout) ->
    gen_fsm:sync_send_event(Pid, {request, Method, Params}, Timeout).

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
    {stop, {Error, Reason}, State};

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
connecting(_Event, State=#state{reconnect=false}) ->
    {stop, {error, no_reconnect}, State};
connecting(_Event, State=#state{reconnect_count=Limit, reconnect_limit=Limit}) ->
    {stop, {error, reconnect_limit}, State};

connecting(timeout, State) ->
    try_connect(State);

connecting({notify, Method, _Params}, State=#state{failfast=true}) ->
    try_connect(inc_stats(Method, [notify, failfast], 1, State));
connecting({notify, Method, _Params}, State=#state{failfast=false, failfast_count=Limit, failfast_limit=Limit}) ->
    {stop, {error, failfast_limit}, inc_stats(Method, [notify, failfast], 1, State)};
connecting(Msg = {notify, Method, _Params}, State=#state{failfast=false, failfast_count=Count, failfast_queue=Queue}) ->
    State2 = State#state{failfast_count=Count+1, failfast_queue=queue:in({Msg, undefined}, Queue)},
    try_connect(inc_stats(Method, [notify, queue], 1, State2));

connecting(_Event, State) ->
    {next_state, connecting, State}.

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
connecting(_Event, _From, State=#state{reconnect=false}) ->
    {stop, {error, no_reconnect}, {error, no_connection}, State};
connecting(_Event, _From, State=#state{reconnect_count=Limit, reconnect_limit=Limit}) ->
    {stop, {error, reconnect_limit}, {error, no_connection}, State};
connecting(_Event, _From, State=#state{failfast_count=Limit, failfast_limit=Limit}) ->
    {stop, {error, failfast_limit}, {error, no_connection}, State};

connecting({join, Req}, _From, State) ->
    Method = msgpack_rpc_request:get(method, Req),
    {reply, {error, no_connection}, connecting, inc_stats(Method, [join, failfast], 1, State), 0};

connecting({request, Method, _Params}, _From, State=#state{failfast=true}) ->
    {reply, {error, no_connection}, connecting, inc_stats(Method, [request, failfast], 1, State), 0};
connecting(Msg = {request, Method, _Params}, From, State=#state{failfast=false, failfast_count=Count, failfast_queue=Queue}) ->
    State2 = State#state{failfast_count=Count+1, failfast_queue=queue:in({Msg, From}, Queue)},
    try_connect(inc_stats(Method, [request, queue], 1, State2));

connecting(_Event, _From, State) ->
    {next_state, connecting, State}.

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
ready(timeout, State=#state{failfast_queue=Queue}) ->
    case queue:out(Queue) of
        {{value, {Msg, undefined}}, Queue2} ->
            gen_fsm:send_event(self(), Msg),
            {next_state, ready, State#state{failfast_queue=Queue2}, 0};
        {{value, {Msg, From}}, Queue2} ->
            Self = self(),
            spawn(fun() ->
                Reply = gen_fsm:sync_send_event(Self, Msg),
                gen_fsm:reply(From, Reply)
            end),
            {next_state, ready, State#state{failfast_queue=Queue2}, 0};
        {empty, Queue} ->
            {next_state, ready, State}
    end;

ready({notify, Method, Params}, State) ->
    do_notify(State, {Method, Params});

ready(_Event, State) ->
    {next_state, ready, State}.

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
ready({join, Req}, From, State) ->
    do_join(State, From, Req);

ready({request, Method, Params}, From, State=#state{counter=MsgId}) ->
    do_request(State#state{counter=MsgId+1}, From, {MsgId, Method, Params});

ready(_Event, _From, State) ->
    {next_state, ready, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
do_join(State=#state{session=Session}, From, Req) ->
    MsgId = msgpack_rpc_request:get(msg_id, Req),
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
        options=#msgpack_rpc_options{msgpack_packer=Packer}}, {Method, Params}) ->
    Start = erlang:now(),
    Req = msgpack_rpc_notify:new(Method, Params),
    try
        Object = msgpack_rpc_notify:to_msgpack_object(Req),
        case Packer(Object) of
            {error, Reason} ->
                erlang:error(Reason);
            Packet ->
                case Transport:send(Socket, Packet) of
                    ok ->
                        Time = timer:now_diff(erlang:now(), Start),
                        {next_state, ready, inc_stats(Method, [notify, send], Time, State), 0};
                    {error, SocketReason} ->
                        erlang:error(SocketReason)
                end
        end
    catch
        Class:Error ->
            error_logger:warning_msg(
                "** ~p client ~p non-fatal error in ~p/~p~n"
                "   for the reason ~p:~p~n** Message was ~p~n"
                "** Stacktrace: ~p~n~n",
                [?MODULE, self(), do_notify, 2, Class, Error, Req, erlang:get_stacktrace()]),
            ErrorTime = timer:now_diff(erlang:now(), Start),
            State2 = inc_stats(Method, [notify, error], ErrorTime, State),
            case State2#state.failfast of
                true ->
                    {next_state, connecting, inc_stats(Method, [notify, failfast], 1, State2), 0};
                false ->
                    Count = State2#state.failfast_count + 1,
                    Queue = queue:in({{notify, Method, Params}, undefined}, State2#state.failfast_queue),
                    {next_state, connecting, State2#state{failfast_count=Count, failfast_queue=Queue}, 0}
            end
    end.

%% @private
do_request(State=#state{socket=Socket, transport=Transport, session=Session,
        options=#msgpack_rpc_options{msgpack_packer=Packer}}, From, {MsgId, Method, Params}) ->
    Start = erlang:now(),
    Req = msgpack_rpc_request:new(MsgId, Method, Params),
    try
        Object = msgpack_rpc_request:to_msgpack_object(Req),
        case Packer(Object) of
            {error, PackerReason} ->
                erlang:error(PackerReason);
            Packet ->
                case Transport:send(Socket, Packet) of
                    ok ->
                        Time = timer:now_diff(erlang:now(), Start),
                        Call = #call{msg_id=MsgId, request=Req},
                        Transport:setopts(Socket, [{active, once}]),
                        {reply, {ok, Req}, ready, inc_stats(Method, [request, send], Time, State#state{session=[Call | Session]}), 0};
                    {error, SocketReason} ->
                        erlang:error(SocketReason)
                end
        end
    catch
        Class:Reason ->
            error_logger:warning_msg(
                "** ~p client ~p non-fatal error in ~p/~p~n"
                "   for the reason ~p:~p~n** Message was ~p~n"
                "** Stacktrace: ~p~n~n",
                [?MODULE, self(), do_request, 3, Class, Reason, Req, erlang:get_stacktrace()]),
            ErrorTime = timer:now_diff(erlang:now(), Start),
            State2 = inc_stats(Method, [request, error], ErrorTime, State),
            case State2#state.failfast of
                true ->
                    {reply, {error, Reason}, connecting, inc_stats(Method, [request, failfast], 1, State2), 0};
                false ->
                    Count = State2#state.failfast_count + 1,
                    Queue = queue:in({{request, Method, Params}, From}, State2#state.failfast_queue),
                    {next_state, connecting, State2#state{failfast_count=Count, failfast_queue=Queue}, 0}
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
    {FsmOpts, Opts} = msgpack_rpc:partition_options(OrigOpts, [
        debug, failfast, failfast_limit,
        reconnect, reconnect_limit, reconnect_min, reconnect_max,
        stats, transport, transport_options]),

    State = #state{client=Client},

    Debug = proplists:get_value(debug, FsmOpts, State#state.debug),
    Failfast = proplists:get_value(failfast, FsmOpts, State#state.failfast),
    FailfastLim = proplists:get_value(failfast_limit, FsmOpts, State#state.failfast_limit),
    Reconnect = proplists:get_value(reconnect, FsmOpts, State#state.reconnect),
    ReconnLim = proplists:get_value(reconnect_limit, FsmOpts, State#state.reconnect_limit),
    ReconnMin = proplists:get_value(reconnect_min, FsmOpts, State#state.reconnect_min),
    ReconnMax = proplists:get_value(reconnect_max, FsmOpts, State#state.reconnect_max),
    Stats = proplists:get_value(stats, FsmOpts, State#state.stats),
    TransOpts = proplists:get_value(transport_options, FsmOpts, []),
    Transport = msgpack_rpc:known_transport(proplists:get_value(transport, FsmOpts, ranch_tcp)),

    Options = msgpack_rpc_options:new(Opts),

    State#state{messages=Transport:messages(), options=Options,
        debug=Debug, failfast=Failfast, failfast_limit=FailfastLim,
        reconnect=Reconnect, reconnect_limit=ReconnLim,
        reconnect_min=ReconnMin, reconnect_max=ReconnMax,
        stats=Stats, transport=Transport, transport_opts=TransOpts}.

%% @private
reconnect_time(#state{reconnect_min=Min, reconnect_time=0}) ->
    Min;
reconnect_time(#state{reconnect_max=Max, reconnect_time=Max}) ->
    Max;
reconnect_time(#state{reconnect_max=Max, reconnect_time=R}) ->
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
        transport_opts=TransportOpts, reconnect_count=ReconnCount,
        client=#msgpack_rpc_client{address=Address, port=Port}}) ->
    case OldSocket of
        undefined -> ok;
        _         -> (catch Transport:close(OldSocket))
    end,
    case Transport:connect(Address, Port, TransportOpts) of
        {ok, Socket} ->
            Transport:controlling_process(Socket, self()),
            ok = Transport:setopts(Socket, [{active, once}]),
            {next_state, ready, State#state{socket=Socket, failfast_count=0, reconnect_count=0, reconnect_time=0}, 0};
        {E, Reason} when E == error; E == exception ->
            ReconnTime = reconnect_time(State),
            case State#state.debug of
                true ->
                    error_logger:warning_msg("[~w] ~w connect failed (~w), trying again in ~w ms~n",
                        [self(), ?MODULE, Reason, ReconnTime]);
                _ ->
                    ok
            end,
            {next_state, connecting, State#state{socket=undefined, reconnect_count=ReconnCount+1, reconnect_time=ReconnTime}, ReconnTime}
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
