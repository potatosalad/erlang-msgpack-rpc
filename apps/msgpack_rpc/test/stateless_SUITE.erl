-module(stateless_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests.
-export([notify/1]).
-export([request/1]).

all() ->
    [
        {group, stateless_server}
    ].

groups() ->
    Tests = [
        notify,
        request
    ],
    [
        {stateless_server, [parallel], Tests}
    ].

init_per_suite(Config) ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(ranch),
    application:start(msgpack_rpc_server),
    Config.

end_per_suite(_Config) ->
    application:stop(msgpack_rpc_server),
    application:stop(ranch),
    application:stop(ssl),
    application:stop(public_key),
    application:stop(crypto),
    ok.

init_per_group(stateless_server, Config) ->
    ServerName = proplists:get_value(server_name, Config, stateless_server),
    ServerOpts = proplists:get_value(server_opts, Config, []),
    Transport = ranch_tcp,
    TransOpts = [],
    Config2 = lists:keystore(server_name, 1, Config, {server_name, ServerName}),
    Config3 = lists:keystore(server_opts, 1, Config2, {server_opts, ServerOpts}),
    Config4 = lists:keystore(transport, 1, Config3, {transport, Transport}),
    {port, Port} = test_helper:server_start(Config4, TransOpts),
    [{port, Port} | Config4].

end_per_group(Name, _) ->
    msgpack_rpc:stop_listener(Name),
    ok.

%%====================================================================
%% Tests
%%====================================================================

notify(Config) ->
    {ok, Specs} = load_test_specs(notify, Config),
    {ok, Client} = test_helper:client_connect(Config, []),
    test_helper:client_notify_specs(Client, Specs),
    ok = msgpack_rpc_client:close(Client).

request(Config) ->
    {ok, Specs} = load_test_specs(request, Config),
    {ok, Client} = test_helper:client_connect(Config, []),
    test_helper:client_call_specs(Client, Specs),
    test_helper:client_call_async_specs(Client, Specs),
    ok = msgpack_rpc_client:close(Client).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

load_test_specs(Test, Config) ->
    Spec = filename:join(?config(data_dir, Config), atom_to_list(Test) ++ ".spec"),
    file:consult(Spec).
