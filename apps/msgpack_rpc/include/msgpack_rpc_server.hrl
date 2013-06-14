-record(msgpack_rpc_task, {
    type    = undefined :: undefined | request | notify,
    message = undefined :: undefined | msgpack_rpc:request() | msgpack_rpc:notify(),
    options = undefined :: undefined | msgpack_rpc:options(),

    %% Response
    fsm_pid   = undefined :: undefined | pid(),
    response  = undefined :: undefined | msgpack_rpc:response(),
    socket    = undefined :: undefined | inet:socket(),
    transport = undefined :: undefined | module()
}).
