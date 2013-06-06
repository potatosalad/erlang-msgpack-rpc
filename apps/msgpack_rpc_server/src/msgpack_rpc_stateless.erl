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
-export([init/2,
         handle_request/2,
         handle_notify/2,
         handle_info/2,
         terminate/3]).

%% Task API.
-export([request_task/1,
         notify_task/1]).

-record(state, {
    service = undefined :: undefined | module()
}).

init(_Transport, Opts) ->
    case proplists:get_value(service, Opts, undefined) of
        undefined ->
            error_logger:error_msg("no service defined"),
            {error, no_service_defined};
        Service ->
            {ok, #state{service=Service}}
    end.

handle_request(Req, State=#state{service=Service}) ->
    [Method, Params] = msgpack_rpc_request:get([method, params], Req),
    Function = binary_to_existing_atom(Method, latin1),
    Task = {?MODULE, request_task, [Service, Function, Params, Req]},
    {ok, Req2} = msgpack_rpc_request:execute(Task, Req),
    {ok, Req2, State}.

handle_notify(Req, State=#state{service=Service}) ->
    [Method, Params] = msgpack_rpc_notify:get([method, params], Req),
    Function = binary_to_existing_atom(Method, latin1),
    Task = {?MODULE, notify_task, [Service, Function, Params]},
    {ok, Req2} = msgpack_rpc_notify:execute(Task, Req),
    {ok, Req2, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(Reason, Req, State) ->
    io:format("[~p] TERMINATE: ~p ~p ~p~n", [?MODULE, Reason, Req, State]),
    ok.

%%====================================================================
%% Task API functions
%%====================================================================

request_task([Service, Method, Params, Req]) ->
    {result, erlang:apply(Service, Method, Params), Req}.

notify_task([Service, Method, Params]) ->
    erlang:apply(Service, Method, Params),
    ok.
