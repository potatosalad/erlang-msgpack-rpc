-module(proper_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests.
-export([proper/1]).
-export([proper_jiffy/1]).
-export([proper_jsx/1]).
-export([proper_nif/1]).

all() ->
    [
        {group, proper_tcp},
        {group, proper_tcp_jiffy},
        {group, proper_tcp_jsx},
        {group, proper_tcp_nif}
    ].

groups() ->
    Tests = [
        proper,
        proper_jiffy,
        proper_jsx,
        proper_nif
    ],
    [
        {proper_tcp, [sequence], Tests},
        {proper_tcp_jiffy, [sequence], Tests},
        {proper_tcp_jsx, [sequence], Tests},
        {proper_tcp_nif, [sequence], Tests}
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

init_per_group(proper_tcp, Config) ->
    ServerName = proplists:get_value(server_name, Config, proper_tcp),
    ServerOpts = proplists:get_value(server_opts, Config, []),
    Transport = ranch_tcp,
    TransOpts = [],
    Config2 = lists:keystore(server_name, 1, Config, {server_name, ServerName}),
    Config3 = lists:keystore(server_opts, 1, Config2, {server_opts, ServerOpts}),
    Config4 = lists:keystore(transport, 1, Config3, {transport, Transport}),
    {port, Port} = test_helper:server_start(Config4, TransOpts),
    [{port, Port} | Config4];
init_per_group(proper_tcp_jiffy, Config) ->
    init_per_group(proper_tcp, [{server_name, proper_tcp_jiffy}, {msgpack_type, jiffy} | Config]);
init_per_group(proper_tcp_jsx, Config) ->
    init_per_group(proper_tcp, [{server_name, proper_tcp_jsx}, {msgpack_type, jsx} | Config]);
init_per_group(proper_tcp_nif, Config) ->
    init_per_group(proper_tcp, [{server_name, proper_tcp_nif}, {msgpack_type, nif} | Config]).

end_per_group(Name, _) ->
    msgpack_rpc:stop_listener(Name),
    ok.

%%====================================================================
%% Tests
%%====================================================================

proper(Config) ->
    run_proper(jiffy, Config).

proper_jiffy(Config) ->
    run_proper(jiffy, lists:keystore(msgpack_type, 1, Config, {msgpack_type, jiffy})).

proper_jsx(Config) ->
    run_proper(jsx, lists:keystore(msgpack_type, 1, Config, {msgpack_type, jsx})).

proper_nif(Config) ->
    run_proper(jiffy, lists:keystore(msgpack_type, 1, Config, {msgpack_type, nif})).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

run_proper(_Spec, Config) ->
    {ok, Client} = test_helper:client_connect(Config, []),
    Config2 = test_helper:set_config(?MODULE, [{client, Client} | Config]),
    [] = proper:module(msgpack_rpc_props),
    test_helper:unset_config(Config2),
    ok = msgpack_rpc_client:close(Client).
