-include_lib("msgpack_rpc/include/msgpack_rpc.hrl").

-record(msgpack_rpc_client, {
    connection = undefined :: undefined | pid()
}).
