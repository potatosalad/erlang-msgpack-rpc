-module(test_helper).

%% Config helpers
-export([get_config/1,
         set_config/2,
         unset_config/1]).

%% ETS table helpers
-export([table_create/3,
         table_delete/1,
         table_exists/1,
         table_insert/3,
         table_lookup/2]).

%% msgpack helpers
-export([msgpack_type/1,
         msgpack_jiffy_pack/1,
         msgpack_jiffy_unpack_stream/1,
         msgpack_jsx_pack/1,
         msgpack_jsx_unpack_stream/1,
         msgpack_nif_pack/1,
         msgpack_nif_unpack_stream/1]).

%% msgpack_rpc_client helpers
-export([client_connect/2,
         client_call_specs/2,
         client_call_async_specs/2,
         client_notify_specs/2]).

%% msgpack_rpc_server helpers
-export([server_start/2,
         server_start_stateless/4]).

get_config(Table) when is_atom(Table) ->
    case table_lookup(Table, config) of
        [{config, Config}] ->
            {ok, Config};
        Other ->
            {error, Other}
    end;
get_config(Config) when is_list(Config) ->
    get_config(proplists:get_value(table, Config, undefined)).

set_config(Table, Config) ->
    Config2 = lists:keystore(table, 1, Config, {table, Table}),
    Table = table_insert(Table, config, Config2),
    Config2.

unset_config(Table) when is_atom(Table) ->
    table_delete(Table);
unset_config(Config) when is_list(Config) ->
    unset_config(proplists:get_value(table, Config, undefined)).

%% @doc Creates ETS table for internal cache if it does not exist yet,
%% otherwise the name of the table is returned.
table_create(TableName, Type, Keypos) ->
    case table_exists(TableName) of
        false ->
            ets:new(TableName, [Type, public, named_table, {keypos,
                Keypos}]);
        true -> TableName
    end.

%% @doc Delete the specified `TableName' ETS table.
table_delete(TableName) ->
    case table_exists(TableName) of
        true ->
            ets:delete(TableName);
        false ->
            true
    end.

%% @doc Checks if ETS table with name `TableName' exists.
table_exists(TableName) ->
    case ets:info(TableName) of
        undefined -> false;
        _TableInfo -> true
    end.

table_insert(Table, Key, Value) ->
    Table = table_create(Table, set, 1),
    ets:insert(Table, {Key, Value}),
    Table.

table_lookup(Table, Key) ->
    Table = table_create(Table, set, 1),
    ets:lookup(Table, Key).

msgpack_type(jiffy) ->
    {fun ?MODULE:msgpack_jiffy_pack/1, fun ?MODULE:msgpack_jiffy_unpack_stream/1};
msgpack_type(jsx) ->
    {fun ?MODULE:msgpack_jsx_pack/1, fun ?MODULE:msgpack_jsx_unpack_stream/1};
msgpack_type(nif) ->
    {fun ?MODULE:msgpack_nif_pack/1, fun ?MODULE:msgpack_nif_unpack_stream/1};
msgpack_type(_) ->
    undefined.

msgpack_jiffy_pack(Term) ->
    msgpack_jiffy:pack(Term).

msgpack_jiffy_unpack_stream(Data) ->
    try
        msgpack_jiffy:unpack(Data)
    catch
        throw:Exception ->
            {error, Exception}
    end.

msgpack_jsx_pack(Term) ->
    msgpack_jsx:pack(Term).

msgpack_jsx_unpack_stream(Data) ->
    try
        msgpack_jsx:unpack(Data)
    catch
        throw:Exception ->
            {error, Exception}
    end.

msgpack_nif_pack(Term) ->
    msgpack_nif:pack(Term).

msgpack_nif_unpack_stream(Data) ->
    msgpack_nif:unpack_stream(Data).

client_connect(Config, Opts) ->
    MsgpackType = proplists:get_value(msgpack_type, Config),
    Cert = proplists:get_value(cert, Config, undefined),
    Key  = proplists:get_value(key,  Config, undefined),
    Port = proplists:get_value(port, Config, undefined),
    Opts2 = case {Cert, Key} of
        {undefined, undefined} ->
            [{transport, tcp} | Opts];
        _ ->
            [{cert, Cert}, {key, Key}, {transport, ssl} | Opts]
    end,
    Opts3 = case test_helper:msgpack_type(MsgpackType) of
        {Packer, Unpacker} ->
            [{msgpack_packer, Packer},
             {msgpack_unpacker, Unpacker} | Opts2];
        _ ->
            Opts2
    end,
    msgpack_rpc_client:connect(localhost, Port, Opts3).

client_call_specs(Client, Specs) ->
    _ = [Ret = begin
        ct:log("CALL~n~n** Method: ~p~n** Params: ~p~n** Expected: ~p~n", [Method, Params, Ret]),
        msgpack_rpc_client:call(Method, Params, Client)
    end || {Method, Params, Ret} <- Specs],
    ok.

client_call_async_specs(Client, Specs) ->
    CallsAsync = [begin
        % ct:log("CALL_ASYNC~n~n** Method: ~p~n** Params: ~p~n", [Method, Params]),
        {ok, Req} = msgpack_rpc_client:call_async(Method, Params, Client),
        {Req, Ret}
    end || {Method, Params, Ret} <- Specs],
    _ = [Ret = begin
        % ct:log("JOIN~n~n** Request: ~p~n** Expected: ~p~n", [Req, Ret]),
        msgpack_rpc_client:join(Req, Client)
    end || {Req, Ret} <- CallsAsync],
    ok.

client_notify_specs(Client, Specs) ->
    RawFrom = term_to_binary(self()),
    _ = [Ret = begin
        ct:log("NOTIFY~n~n** Method: ~p~n** Params: ~p~n", [Method, [RawFrom | Params]]),
        ok = msgpack_rpc_client:notify(Method, [RawFrom | Params], Client),
        receive Ret -> Ret after 1000 -> error end
    end || {Method, Params, Ret} <- Specs],
    ok.

server_start(Config, TransOpts) ->
    MsgpackType = proplists:get_value(msgpack_type, Config),
    Transport = proplists:get_value(transport, Config),
    ServerName = proplists:get_value(server_name, Config),
    ServerOpts = proplists:get_value(server_opts, Config, []),
    ProtoOpts = case test_helper:msgpack_type(MsgpackType) of
        {Packer, Unpacker} ->
            [{msgpack_packer, Packer},
             {msgpack_unpacker, Unpacker} | ServerOpts];
        _ ->
            ServerOpts
    end,
    server_start_stateless(ServerName, Transport, TransOpts, ProtoOpts).

server_start_stateless(Name, Transport, TransOpts, ProtoOpts) ->
    case msgpack_rpc_server:start_listener(Name, 4, Transport,
        [{port, 0} | TransOpts],
        [{handler, msgpack_rpc_stateless},
         {handler_opts, [{service, stateless_service}]} | ProtoOpts]) of
        {ok, _} ->
            {port, ranch:get_port(Name)};
        Error ->
            Error
    end.
