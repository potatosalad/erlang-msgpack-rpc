-module(msgpack_rpc_props).

-include_lib("proper/include/proper.hrl").

-define(SUITE, proper_SUITE).

prop_roundtrips_msgpack_types() ->
    ProperFun = get_proper_fun(?SUITE),
    {{ok, Client}, After} = get_client(?SUITE),
    Ret = numtests(5,
        ?FORALL(Term, ProperFun(),
            begin
                case msgpack_rpc_client:call(echo, [Term], Client) of
                    {ok, Term} ->
                        true;
                    _ ->
                        false
                end
            end)),
    After(Client),
    Ret.

get_client(Suite) ->
    Close = fun(C) ->
        ok = msgpack_rpc_client:close(C)
    end,
    Noop = fun(_) ->
        ok
    end,
    case test_helper:get_config(Suite) of
        {ok, Config} ->
            case proplists:get_value(client, Config, undefined) of
                undefined ->
                    Port = proplists:get_value(port, Config, 65123),
                    {msgpack_rpc_client:connect("localhost", Port, []), Close};
                Client ->
                    {{ok, Client}, Noop}
            end;
        _ ->
            {msgpack_rpc_client:connect("localhost", 65123, []), Close}
    end.

get_proper_fun(Suite) ->
    MsgpackType = case test_helper:get_config(Suite) of
        {ok, Config} ->
            proplists:get_value(msgpack_type, Config, jiffy);
        _ ->
            jiffy
    end,
    case MsgpackType of
        jsx ->
            ct:log("msgpack_type: ~p, using msgpack_rpc_proper:choose_type_jsx/0~n", [MsgpackType]),
            fun msgpack_rpc_proper:choose_type_jsx/0;
        _ ->
            ct:log("msgpack_type: ~p, using msgpack_rpc_proper:choose_type_jiffy/0~n", [MsgpackType]),
            fun msgpack_rpc_proper:choose_type_jiffy/0
    end.
