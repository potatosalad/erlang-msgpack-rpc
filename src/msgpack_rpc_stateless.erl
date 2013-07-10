%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  31 May 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------

-module(msgpack_rpc_stateless).
-behaviour(msgpack_rpc_handler).

%% msgpack_rpc_handler callbacks
-export([msgpack_rpc_init/2,
         msgpack_rpc_request/3,
         msgpack_rpc_notify/2,
         msgpack_rpc_info/2,
         msgpack_rpc_terminate/3]).

-record(state, {
    service = undefined :: undefined | module()
}).

%%%===================================================================
%%% msgpack_rpc_handler callbacks
%%%===================================================================

msgpack_rpc_init(_Type, Opts) ->
    case msgpack_rpc_util:get_value(service, Opts, undefined) of
        undefined ->
            erlang:error(no_service_defined);
        Service ->
            {ok, #state{service=Service}}
    end.

msgpack_rpc_request({_MsgId, Method, Params}, From, State=#state{service=Service}) ->
    supervisor:start_child(msgpack_rpc_stateless_fsm_sup, [Service, Method, Params, From]),
    {noreply, State}.

msgpack_rpc_notify({Method, Params}, State=#state{service=Service}) ->
    supervisor:start_child(msgpack_rpc_stateless_fsm_sup, [Service, Method, Params]),
    {noreply, State}.

msgpack_rpc_info(_Info, State) ->
    {noreply, State}.

msgpack_rpc_terminate(_Reason, _Req, _State) ->
    ok.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
