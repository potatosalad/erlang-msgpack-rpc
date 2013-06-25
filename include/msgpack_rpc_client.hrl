-record(msgpack_rpc_client, {
    %% Settings
    address = undefined :: undefined | inet:ip_address() | inet:hostname(),
    port    = undefined :: undefined | inet:port_number(),
    options = undefined :: undefined | [proplists:property()],

    %% FSM
    fsm_opts = undefined :: undefined | [proplists:property()],
    fsm_pid  = undefined :: undefined | pid(),

    %% Options
    timeout = 10000 :: timeout()
}).
