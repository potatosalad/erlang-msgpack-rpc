%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  31 May 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------

-module(msgpack_rpc_stateless_fsm).
-behaviour(gen_fsm).

-include("msgpack_rpc.hrl").
-include("msgpack_rpc_server.hrl").

%% API
-export([start_link/3, start_link/4]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-export([execute/2]).

-record(state, {
    m = undefined :: undefined | module(),
    f = undefined :: undefined | atom(),
    a = undefined :: undefined | [any()],
    from = undefined :: undefined | {pid(), any()}
}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Service, Method, Params) ->
    gen_fsm:start_link(?MODULE, {Service, Method, Params}, []).

start_link(Service, Method, Params, From) ->
    gen_fsm:start_link(?MODULE, {Service, Method, Params, From}, []).

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
init({Service, Method, Params}) ->
    case catch binary_to_existing_atom(Method, utf8) of
        Function when is_atom(Function) ->
            State = #state{m=Service, f=Function, a=Params},
            {ok, execute, State, 0};
        _ ->
            {stop, normal}
    end;
init({Service, Method, Params, From}) ->
    case catch binary_to_existing_atom(Method, utf8) of
        Function when is_atom(Function) ->
            State = #state{m=Service, f=Function, a=Params, from=From},
            {ok, execute, State, 0};
        _ ->
            msgpack_rpc:reply(From, {error, undef}),
            {stop, normal}
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
terminate(Reason, StateName, State=#state{from=From}) when From =/= undefined ->
    msgpack_rpc:reply(From, {error, Reason}),
    terminate(Reason, StateName, State#state{from=undefined});
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
%% @spec execute(Event :: term(), StateData :: term()) ->
%%     {next_state, NextStateName :: atom(), NewStateData :: term()} |
%%     {next_state, NextStateName :: atom(), NewStateData :: term(), timeout() | hibernate} |
%%     {stop, Reason :: normal | shutdown | term(), NewStateData :: term()}
%% @end
%%--------------------------------------------------------------------
execute(timeout, State=#state{m=M, f=F, a=A, from=From}) ->
    try
        Result = erlang:apply(M, F, A),
        msgpack_rpc:reply(From, {result, Result}),
        {stop, normal, State#state{from=undefined}}
    catch
        _:Reason ->
           msgpack_rpc:reply(From, {error, Reason}),
           {stop, normal, State#state{from=undefined}}
    end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
