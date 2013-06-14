-module(msgpack_rpc_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests.
% -export([client_request/1]).
% -export([client_notify/1]).
% -export([identity_test/1]).
% -export([property_test/1]).

all() ->
    [].
    % [
    %     {group, stateless_tcp_server},
    %     {group, stateless_ssl_server},
    %     {group, stateless_jsx_server},
    %     {group, stateless_jiffy_server},
    %     {group, stateless_nif_server}
    % ].

groups() ->
    Tests = [
        client_request,
        client_notify,
        identity_test%,
        % property_test
    ],
    [
        {stateless_tcp_server, [parallel], Tests},
        {stateless_ssl_server, [parallel], Tests},
        {stateless_jsx_server, [parallel], Tests},
        {stateless_jiffy_server, [parallel], Tests},
        {stateless_nif_server, [parallel], Tests}
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

init_per_group(stateless_tcp_server, Config) ->
    Transport = ranch_tcp,
    {ok, _} = msgpack_rpc_server:start(stateless_tcp_server, 4, tcp, stateless_service, [{port, 0}]),
    Port = ranch:get_port(stateless_tcp_server),
    [{port, Port}, {transport, Transport} | Config];
init_per_group(stateless_ssl_server, Config) ->
    Transport = ranch_ssl,
    {_, Cert, Key} = ct_helper:make_certs(),
    {ok, _} = msgpack_rpc_server:start(stateless_ssl_server, 4, ssl, stateless_service, [{port, 0}, {cert, Cert}, {key, Key}]),
    Port = ranch:get_port(stateless_ssl_server),
    [{port, Port}, {transport, Transport}, {cert, Cert}, {key, Key} | Config];
init_per_group(stateless_jsx_server, Config) ->
    MsgpackType = jsx,
    Transport = ranch_tcp,
    {Packer, Unpacker} = test_helper:msgpack_type(MsgpackType),
    {ok, _} = msgpack_rpc_server:start_listener(stateless_jsx_server, 4, ranch_tcp, [{port, 0}],
        [{handler, msgpack_rpc_stateless},
         {handler_opts, [{service, stateless_service}]},
         {msgpack_packer, Packer},
         {msgpack_unpacker, Unpacker}]),
    Port = ranch:get_port(stateless_jsx_server),
    [{msgpack_type, MsgpackType}, {port, Port}, {transport, Transport} | Config];
init_per_group(stateless_jiffy_server, Config) ->
    MsgpackType = jiffy,
    Transport = ranch_tcp,
    {Packer, Unpacker} = test_helper:msgpack_type(MsgpackType),
    {ok, _} = msgpack_rpc_server:start_listener(stateless_jiffy_server, 4, ranch_tcp, [{port, 0}],
        [{handler, msgpack_rpc_stateless},
         {handler_opts, [{service, stateless_service}]},
         {msgpack_packer, Packer},
         {msgpack_unpacker, Unpacker}]),
    Port = ranch:get_port(stateless_jiffy_server),
    [{msgpack_type, MsgpackType}, {port, Port}, {transport, Transport} | Config];
init_per_group(stateless_nif_server, Config) ->
    MsgpackType = nif,
    Transport = ranch_tcp,
    {Packer, Unpacker} = test_helper:msgpack_type(MsgpackType),
    {ok, _} = msgpack_rpc_server:start_listener(stateless_nif_server, 4, ranch_tcp, [{port, 0}],
        [{handler, msgpack_rpc_stateless},
         {handler_opts, [{service, stateless_service}]},
         {msgpack_packer, Packer},
         {msgpack_unpacker, Unpacker}]),
    Port = ranch:get_port(stateless_nif_server),
    [{msgpack_type, MsgpackType}, {port, Port}, {transport, Transport} | Config].

end_per_group(Name, _) ->
    msgpack_rpc:stop_listener(Name),
    ok.

% client_request(Config) ->
%     {ok, Specs} = load_test_specs(client_request, Config),
%     {ok, Client} = connect_client(Config, []),
%     specs_call(Client, Specs),
%     specs_call_async(Client, Specs),
%     ok = msgpack_rpc_client:close(Client).

% client_notify(Config) ->
%     {ok, Specs} = load_test_specs(client_notify, Config),
%     {ok, Client} = connect_client(Config, []),
%     specs_notify(Client, Specs),
%     ok = msgpack_rpc_client:close(Client).

% identity_test(Config) ->
%     {ok, [Terms]} = load_test_specs(identity, Config),
%     {ok, Client} = connect_client(Config, []),
%     Specs = [{echo, [Term], {ok, Term}} || Term <- Terms],
%     specs_call(Client, Specs),
%     specs_call_async(Client, Specs),
%     ok = msgpack_rpc_client:close(Client).

% property_test(Config) ->
%     {ok, Client} = connect_client(Config, [{timeout, 120000}]),
%     Config2 = test_helper:set_config(?MODULE, [{client, Client} | Config]),
%     [] = proper:module(msgpack_rpc_props),
%     test_helper:unset_config(Config2),
%     ok = msgpack_rpc_client:close(Client).

%% Internal utilities.

% connect_client(Config, Opts) ->
%     MsgpackType = ?config(msgpack_type, Config),
%     {Packer, Unpacker} = test_helper:msgpack_type(MsgpackType),
%     Cert = proplists:get_value(cert, Config, undefined),
%     Key  = proplists:get_value(key,  Config, undefined),
%     Port = proplists:get_value(port, Config, undefined),
%     Opts2 = case {Cert, Key} of
%         {undefined, undefined} ->
%             [{transport, tcp} | Opts];
%         _ ->
%             [{cert, Cert}, {key, Key}, {transport, ssl} | Opts]
%     end,
%     msgpack_rpc_client:connect(localhost, Port,
%         [{msgpack_packer, Packer}, {msgpack_unpacker, Unpacker} | Opts2]).

% load_test_specs(Test, Config) ->
%     Spec = filename:join(?config(data_dir, Config), atom_to_list(Test) ++ ".spec"),
%     file:consult(Spec).
