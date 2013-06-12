%%%-------------------------------------------------------------------
%%% @author UENISHI Kota <kuenishi@gmail.com>
%%% @copyright (C) 2012, UENISHI Kota
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2012 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(msgpack_rpc_connection).
-behaviour(gen_server).

-include("msgpack_rpc_client.hrl").
-include_lib("msgpack_rpc/include/msgpack_rpc_protocol.hrl").

%% API
-export([start_link/4, join/2, notify/3, request/3, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-type ref() :: pid() | #msgpack_rpc_client{}.
-export_type([ref/0]).

-type session() :: [{non_neg_integer(), none | {reply, msgpack:msgpack_term()} | {waiting, [pid()]}}].

-record(state, {
    %% Settings
    transport = undefined :: undefined | atom(),
    address   = undefined :: undefined | inet:ip_address() | inet:hostname(),
    port      = undefined :: undefined | inet:port_number(),
    opts      = undefined :: undefined | [proplists:property()],

    %% Conection
    socket   = undefined :: undefined | inet:socket(),
    messages = undefined :: undefined | {atom(), atom(), atom()},
    counter  = 0         :: msgpack_rpc:msg_id(),
    session  = []        :: session(),
    buffer   = <<>>      :: binary(),

    %% Options
    options     = #msgpack_rpc_options{} :: msgpack_rpc:options(),
    socket_opts = []                     :: [proplists:property()]
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(atom(), inet:ip_address() | inet:hostname(), inet:port_number(), any())
    -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link(Transport, Address, Port, Opts) ->
    gen_server:start_link(?SERVER, [Transport, Address, Port, Opts], []).

% -spec join(pid(), msgpack_rpc:request()) -> ok.
join(Pid, Req) ->
    gen_server:call(Pid, {join, Req}).

-spec notify(pid(), msgpack_rpc:method(), msgpack_rpc:params()) -> ok.
notify(Pid, Method, Params) ->
    gen_server:call(Pid, {notify, Method, Params}).

-spec request(pid(), msgpack_rpc:method(), msgpack_rpc:params())
    -> {ok, msgpack_rpc:request()} | {error, any()}.
request(Pid, Method, Params) ->
    gen_server:call(Pid, {request, Method, Params}).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:call(Pid, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Transport, Address, Port, Opts]) ->
    {ClientOpts, Opts2} = msgpack_rpc:partition_options(Opts, [socket_options]),
    SocketOpts = proplists:get_value(socket_options, ClientOpts, []),
    {ok, Options} = msgpack_rpc_options:from_options(Opts2),
    State = #state{transport=Transport, address=Address, port=Port,
        opts=Opts, messages=Transport:messages(), options=Options,
        socket_opts=SocketOpts},
    io:format("Connection: ~p~n", [State]),
    case connect(State) of
        {ok, State2} ->
            {ok, State2};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({join, Req}, From, State) ->
    do_join(Req, From, State);
handle_call({notify, Method, Params}, _From, State) ->
    Req = msgpack_rpc_notify:new(Method, Params),
    do_notify(Req, State);
handle_call({request, Method, Params}, _From, State=#state{counter=Count}) ->
    Req = msgpack_rpc_request:new(Count, Method, Params),
    do_request(Req, State#state{counter=Count+1});
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error, badevent}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({OK, Socket, Data}, State=#state{socket=Socket, messages={OK, _Closed, _Error}, buffer=SoFar}) ->
    % ok = Transport:setopts(Socket, [{active, once}]),
    parse_data(State, << SoFar/binary, Data/binary >>);
handle_info({Closed, Socket}, State=#state{socket=Socket, messages={_OK, Closed, _Error}}) ->
    {stop, {error, closed}, State};
handle_info({Error, Socket, Reason}, State=#state{socket=Socket, messages={_OK, _Closed, Error}}) ->
    {stop, {error, Reason}, State};
handle_info(_Info, State=#state{socket=Socket, transport=Transport}) ->
    % ?debugVal(_Info),
    %error_logger:error_msg("error: ~p~n", [Reason]),
    ok = Transport:setopts(Socket, [{active, once}]),
    {noreply, State}.

terminate(_Reason, _State=#state{socket=Socket, transport=Transport}) ->
    Transport:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

-spec connect(#state{}) -> {ok, #state{}} | {error, {connection_error, any()}}.
connect(State=#state{transport=Transport, socket_opts=SocketOpts}) ->
    case Transport:connect(State#state.address, State#state.port, SocketOpts) of
        {ok, Socket} ->
            {ok, State#state{socket=Socket}};
        {error, Reason} ->
            {error, {connection_error, Reason}}
    end.

do_join(_Req, _From, State=#state{socket=undefined}) ->
    {reply, {error, no_connection}, State};
do_join(Req, From, State=#state{session=Sessions}) ->
    {MsgId, Req} = msgpack_rpc_request:msg_id(Req),
    case lists:keytake(MsgId, 1, Sessions) of
        false ->
            {reply, {error, request_not_found}, State};
        {value, {MsgId, Req, none}, Sessions2} ->
            Queue = queue:new(),
            {noreply, State#state{session=[{MsgId, Req, {waiting, queue:in(From, Queue)}} | Sessions2]}};
        {value, {MsgId, Req, {waiting, Queue}}, Sessions2} ->
            {noreply, State#state{session=[{MsgId, Req, {waiting, queue:in(From, Queue)}} | Sessions2]}};
        {value, {MsgId, Req, Response}, Sessions2} ->
            {reply, {ok, Response}, State#state{session=Sessions2}};
        _ ->
            {noreply, State}
    end.

do_notify(_Req, State=#state{socket=undefined}) ->
    {reply, {error, no_connection}, State};
do_notify(Req, State=#state{socket=Socket, transport=Transport,
        options=#msgpack_rpc_options{msgpack_packer=Packer}}) ->
    try
        Object = msgpack_rpc_notify:to_msgpack_object(Req),
        case Packer(Object) of
            {error, Reason} ->
                erlang:error(Reason);
            Packet ->
                case Transport:send(Socket, Packet) of
                    ok ->
                        {reply, {ok, Req}, State};
                    {error, SocketReason} ->
                        {reply, {error, SocketReason}, State}
                end
        end
    catch
        Class:TerminateReason ->
            error_logger:warning_msg(
                "** ~p client ~p non-fatal error in ~p/~p~n"
                "   for the reason ~p:~p~n** Message was ~p~n"
                "** Stacktrace: ~p~n~n",
                [?MODULE, self(), do_notify, 2, Class, TerminateReason, Req, erlang:get_stacktrace()]),
            {reply, {error, TerminateReason}, State}
    end.

-spec do_request(Req::msgpack_rpc:request(), #state{}) -> {reply, Reply::any(), #state{}}.
%% @doc: Sends the given request to msgpack_rpc. If we do not have a
%% connection, returns error.
do_request(_Req, #state{socket=undefined} = State) ->
    {reply, {error, no_connection}, State};
do_request(Req, State=#state{socket=Socket, transport=Transport, session=Sessions,
        options=#msgpack_rpc_options{msgpack_packer=Packer}}) ->
    try
        Object = msgpack_rpc_request:to_msgpack_object(Req),
        case Packer(Object) of
            {error, Reason} ->
                erlang:error(Reason);
            Packet ->
                case Transport:send(Socket, Packet) of
                    ok ->
                        {MsgId, Req} = msgpack_rpc_request:msg_id(Req),
                        Transport:setopts(Socket, [{active, once}]),
                        {reply, {ok, Req}, State#state{session=[{MsgId, Req, none} | Sessions]}};
                    {error, SocketReason} ->
                        {reply, {error, SocketReason}, State}
                end
        end
    catch
        Class:TerminateReason ->
            error_logger:warning_msg(
                "** ~p client ~p non-fatal error in ~p/~p~n"
                "   for the reason ~p:~p~n** Message was ~p~n"
                "** Stacktrace: ~p~n~n",
                [?MODULE, self(), do_request, 2, Class, TerminateReason, Req, erlang:get_stacktrace()]),
            {reply, {error, TerminateReason}, State}
    end.

do_response(Response, Queue) ->
    case queue:out(Queue) of
        {{value, From}, Queue2} ->
            gen_server:reply(From, {ok, Response}),
            do_response(Response, Queue2);
        {empty, Queue} ->
            ok
    end.

parse_data(State=#state{socket=Socket, transport=Transport, session=Sessions,
        options=#msgpack_rpc_options{error_decoder=ErrorDecoder, msgpack_unpacker=Unpacker}}, Data) ->
    case Unpacker(Data) of
        {[?MSGPACK_RPC_RESPONSE, MsgId, Error, Result], RemainingData} ->
            case lists:keytake(MsgId, 1, Sessions) of
                false ->
                    {noreply, State};
                {value, {MsgId, Req, none}, Sessions2} ->
                    Response = msgpack_rpc_response:new(Req, MsgId, ErrorDecoder(Error), Result),
                    {noreply, State#state{session=[{MsgId, Req, Response} | Sessions2], buffer=RemainingData}};
                {value, {MsgId, Req, {waiting, Queue}}, Sessions2} ->
                    Response = msgpack_rpc_response:new(Req, MsgId, ErrorDecoder(Error), Result),
                    do_response(Response, Queue),
                    parse_data(State#state{session=Sessions2}, RemainingData)
            end;
        {error, incomplete} ->
            %% Need more data.
            ok = Transport:setopts(Socket, [{active, once}]),
            {noreply, State#state{buffer=Data}};
        {error, {badarg, Reason}} ->
            {stop, {error, Reason}, State#state{buffer=Data}};
        {error, _Reason} ->
            ok = Transport:setopts(Socket, [{active, once}]),
            {noreply, State#state{buffer=Data}}
    end.
