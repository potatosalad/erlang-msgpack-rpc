Common Options
==============
* Error handling
    - [error_decoder](#error_decoder)
    - [error_encoder](#error_encoder)
* MessagePack
    - [msgpack_packer](#msgpack_packer)
    - [msgpack_unpacker](#msgpack_unpacker)

Error handling
--------------

### error_decoder

`error_decoder = fun msgpack_rpc_util:binary_to_known_error/1 :: function()`

Used by the client to translate incoming errors.
For example, by using [binary_to_term/1](http://www.erlang.org/doc/man/erlang.html#binary_to_term-1), full stacktraces can be received.

```erlang
{ok, Client} = msgpack_rpc_client:connect(localhost, 9199,
    [{error_decoder, fun erlang:binary_to_term/1}]),
{error, Error} = msgpack_rpc_client:call(fail_somehow, [], Client).
```

**Note:** the server must also be configured to send the error with [term_to_binary/1](http://www.erlang.org/doc/man/erlang.html#term_to_binary-1).

### error_encoder

`error_encoder = fun msgpack_rpc_util:known_error_to_binary/1 :: function()`

Used by the server to translate outgoing errors.
For example, by using [term_to_binary/1](http://www.erlang.org/doc/man/erlang.html#term_to_binary-1), full stacktraces can be sent.

```erlang
ok = application:start(crypto),
ok = application:start(ranch),
{ok, Pid} = msgpack_rpc_server:start_listener(my_handler_server, 4, ranch_tcp,
    [{port, 9199}],
    [{handler, my_handler},
     {error_encoder, fun erlang:term_to_binary/1}]).
```

MessagePack
-----------

### msgpack_packer

`msgpack_packer = fun msgpack:pack/1 :: function()`

Used by the client and server to pack outgoing MessagePack packets.

```erlang
%% Use nif packer (client)
{ok, Client} = msgpack_rpc_client:connect(localhost, 9199,
    [{msgpack_packer, fun msgpack_nif:pack/1}]).

%% Use nif packer (server)
ok = application:start(crypto),
ok = application:start(ranch),
{ok, Pid} = msgpack_rpc_server:start_listener(my_handler_server, 4, ranch_tcp,
    [{port, 9199}],
    [{handler, my_handler},
     {msgpack_packer, fun msgpack_nif:pack/1}]).
```

### msgpack_unpacker

`msgpack_packer = fun msgpack:unpack_stream/1 :: function()`

Used by the client and server to unpack incoming MessagePack packets.

```erlang
%% Use nif unpacker (client)
{ok, Client} = msgpack_rpc_client:connect(localhost, 9199,
    [{msgpack_unpacker, fun msgpack_nif:unpack_stream/1}]).

%% Use nif unpacker (server)
ok = application:start(crypto),
ok = application:start(ranch),
{ok, Pid} = msgpack_rpc_server:start_listener(my_handler_server, 4, ranch_tcp,
    [{port, 9199}],
    [{handler, my_handler},
     {msgpack_unpacker, fun msgpack_nif:unpack_stream/1}]).
```
