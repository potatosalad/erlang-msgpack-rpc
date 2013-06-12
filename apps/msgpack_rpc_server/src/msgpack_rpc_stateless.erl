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
         msgpack_rpc_handle/3,
         msgpack_rpc_info/2,
         msgpack_rpc_terminate/3]).

%% Internal functions.
-export([exec_notify/1,
         exec_request/1]).

-record(state, {
    service = undefined :: undefined | module()
}).

%%%===================================================================
%%% msgpack_rpc_handler callbacks
%%%===================================================================

msgpack_rpc_init(_Type, Opts) ->
    case proplists:get_value(service, Opts, undefined) of
        undefined ->
            erlang:error(no_service_defined);
        Service ->
            {ok, #state{service=Service}}
    end.

msgpack_rpc_handle({notify, Method, Params}, Task, State=#state{service=Service}) ->
    Job = {?MODULE, exec_notify, [Service, Method, Params]},
    {execute, Job, Task, State};
msgpack_rpc_handle({request, Method, Params}, Task, State=#state{service=Service}) ->
    Job = {?MODULE, exec_request, [Service, Method, Params]},
    {execute, Job, Task, State}.

msgpack_rpc_info(_Info, State) ->
    {ok, State}.

msgpack_rpc_terminate(_Reason, _Message, _State) ->
    ok.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
exec_notify([Service, Method, Params]) ->
    try binary_to_existing_atom(Method, latin1) of
        Function ->
            erlang:apply(Service, Function, Params)
    catch _:_ ->
        erlang:error(undef)
    after
        ok
    end.

%% @private
exec_request([Service, Method, Params]) ->
    try binary_to_existing_atom(Method, latin1) of
        Function ->
            {respond, {result, erlang:apply(Service, Function, Params)}}
    catch _:_ ->
        erlang:error(undef)
    end.
