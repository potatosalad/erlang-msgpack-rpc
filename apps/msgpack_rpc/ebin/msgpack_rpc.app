{application,msgpack_rpc,
             [{description,"MessagePack RPC client/server suite"},
              {vsn,"0.6.2"},
              {modules,[msgpack_rpc,msgpack_rpc_client,
                        msgpack_rpc_connection]},
              {registered,[]},
              {applications,[kernel,stdlib,msgpack_rpc_client,
                             msgpack_rpc_server]},
              {env,[]}]}.
