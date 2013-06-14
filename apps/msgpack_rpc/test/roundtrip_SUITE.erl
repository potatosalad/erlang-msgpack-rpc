-module(roundtrip_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests.
-export([roundtrip/1]).
-export([roundtrip_jiffy/1]).
-export([roundtrip_jsx/1]).
-export([roundtrip_nif/1]).

all() ->
    [
        {group, ssl_stateless},
        {group, tcp_stateless},
        {group, tcp_stateless_jiffy},
        {group, tcp_stateless_jsx},
        {group, tcp_stateless_nif}
    ].

groups() ->
    Tests = [
        roundtrip,
        roundtrip_jiffy,
        roundtrip_jsx,
        roundtrip_nif
    ],
    [
        {ssl_stateless, [parallel], Tests},
        {tcp_stateless, [parallel], Tests},
        {tcp_stateless_jiffy, [parallel], Tests},
        {tcp_stateless_jsx, [parallel], Tests},
        {tcp_stateless_nif, [parallel], Tests}
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

init_per_group(ssl_stateless, Config) ->
    ServerName = proplists:get_value(server_name, Config, ssl_stateless),
    ServerOpts = proplists:get_value(server_opts, Config, []),
    Transport = ranch_ssl,
    {_, Cert, Key} = ct_helper:make_certs(),
    TransOpts = [{cert, Cert}, {key, Key}],
    Config2 = lists:keystore(server_name, 1, Config, {server_name, ServerName}),
    Config3 = lists:keystore(server_opts, 1, Config2, {server_opts, ServerOpts}),
    Config4 = lists:keystore(transport, 1, Config3, {transport, Transport}),
    {port, Port} = test_helper:server_start(Config4, TransOpts),
    [{port, Port}, {cert, Cert}, {key, Key} | Config4];
init_per_group(tcp_stateless, Config) ->
    ServerName = proplists:get_value(server_name, Config, tcp_stateless),
    ServerOpts = proplists:get_value(server_opts, Config, []),
    Transport = ranch_tcp,
    TransOpts = [],
    Config2 = lists:keystore(server_name, 1, Config, {server_name, ServerName}),
    Config3 = lists:keystore(server_opts, 1, Config2, {server_opts, ServerOpts}),
    Config4 = lists:keystore(transport, 1, Config3, {transport, Transport}),
    {port, Port} = test_helper:server_start(Config4, TransOpts),
    [{port, Port} | Config4];
init_per_group(tcp_stateless_jiffy, Config) ->
    init_per_group(tcp_stateless, [{server_name, tcp_stateless_jiffy}, {msgpack_type, jiffy} | Config]);
init_per_group(tcp_stateless_jsx, Config) ->
    init_per_group(tcp_stateless, [{server_name, tcp_stateless_jsx}, {msgpack_type, jsx} | Config]);
init_per_group(tcp_stateless_nif, Config) ->
    init_per_group(tcp_stateless, [{server_name, tcp_stateless_nif}, {msgpack_type, nif} | Config]).

end_per_group(Name, _) ->
    msgpack_rpc:stop_listener(Name),
    ok.

%%====================================================================
%% Tests
%%====================================================================

roundtrip(Config) ->
    run_roundtrip(identity, lists:keydelete(msgpack_type, 1, Config)).

roundtrip_jiffy(Config) ->
    run_roundtrip(jiffy, lists:keystore(msgpack_type, 1, Config, {msgpack_type, jiffy})).

roundtrip_jsx(Config) ->
    run_roundtrip(jsx, lists:keystore(msgpack_type, 1, Config, {msgpack_type, jsx})).

roundtrip_nif(Config) ->
    run_roundtrip(jiffy, lists:keystore(msgpack_type, 1, Config, {msgpack_type, nif})).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

load_test_specs(Test, Config) ->
    Spec = filename:join(?config(data_dir, Config), atom_to_list(Test) ++ ".spec"),
    file:consult(Spec).

run_roundtrip(Spec, Config) ->
    {ok, [Terms]} = load_test_specs(Spec, Config),
    {ok, Client} = test_helper:client_connect(Config, []),
    Specs = [{echo, [Term], {ok, Term}} || Term <- Terms],
    test_helper:client_call_specs(Client, Specs),
    ok = msgpack_rpc_client:close(Client).
