-module(msgpack_rpc_SUITE).

%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests.
-export([client_request/1]).
-export([client_notify/1]).

all() ->
    [
        {group, tcp_server},
        {group, ssl_server}
    ].

groups() ->
    Tests = [
        client_request,
        client_notify
    ],
    [
        {tcp_server, [parallel], Tests},
        {ssl_server, [parallel], Tests}
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

init_per_group(tcp_server, Config) ->
    Transport = ranch_tcp,
    % {ok, _} = msgpack_rpc:start_tcp_listener(tcp_server, 100, [{port, 0}], [{env, [{handler, msgpack_rpc_stateless}, {handler_opts, [{service, stateless_service}]}]}]),
    {ok, _} = msgpack_rpc_server:start(tcp_server, 4, tcp, stateless_service, [{port, 0}]),
    Port = ranch:get_port(tcp_server),
    [{port, Port}, {transport, Transport} | Config];
init_per_group(ssl_server, Config) ->
    Transport = ranch_ssl,
    {_, Cert, Key} = ct_helper:make_certs(),
    {ok, _} = msgpack_rpc_server:start(ssl_server, 4, ssl, stateless_service, [{port, 0}, {cert, Cert}, {key, Key}]),
    Port = ranch:get_port(ssl_server),
    [{port, Port}, {transport, Transport}, {cert, Cert}, {key, Key} | Config].

end_per_group(Name, _) ->
    msgpack_rpc:stop_listener(Name),
    ok.

client_request(Config) ->
    {ok, Client} = connect_client(Config),
    Tests = [
        {ping, [], {ok, <<"pong">>}},
        {echo, [<<"test">>], {ok, <<"test">>}},
        {sum, [1, 2], {ok, 3}},
        {undefined, [], {error, undef}}%,
        % {<<"bad method">>, [], {error, undef}}
    ],
    _ = [Ret = begin
        msgpack_rpc_client:call(Client, Method, Params)
    end || {Method, Params, Ret} <- Tests],
    ok = msgpack_rpc_client:close(Client).

client_notify(Config) ->
    {ok, Client} = connect_client(Config),
    Tests = [
        {ping, [], pong},
        {echo, [<<"test">>], <<"test">>},
        {sum, [1, 2], 3}
    ],
    RawFrom = term_to_binary(self()),
    _ = [Ret = begin
        ok = msgpack_rpc_client:notify(Client, Method, [RawFrom | Params]),
        receive Ret -> Ret after 1000 -> error end
    end || {Method, Params, Ret} <- Tests],
    ok = msgpack_rpc_client:close(Client).

%% Internal utilities.

% connect_client(Config) ->
%     Cert = proplists:get_value(cert, Config, undefined),
%     Key  = proplists:get_value(key,  Config, undefined),
%     Port = proplists:get_value(port, Config, undefined),
%     case {Cert, Key} of
%         {undefined, undefined} ->
%             msgpack_rpc_client:connect(tcp, localhost, Port, []);
%         _ ->
%             msgpack_rpc_client:connect(ssl, localhost, Port, [])
%     end.

connect_client(Config) ->
    Cert = proplists:get_value(cert, Config, undefined),
    Key  = proplists:get_value(key,  Config, undefined),
    Port = proplists:get_value(port, Config, undefined),
    case {Cert, Key} of
        {undefined, undefined} ->
            msgpack_rpc_client:connect(tcp, localhost, Port, []);
        _ ->
            msgpack_rpc_client:connect(ssl, localhost, Port, [{cert, Cert}, {key, Key}])
    end.
