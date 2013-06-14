-record(msgpack_rpc_client, {
    connection = undefined :: undefined | pid(),
    timeout    = 10000     :: timeout()
}).
