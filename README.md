MessagePack-RPC Erlang
======================

[![Build Status](https://travis-ci.org/potatosalad/msgpack-rpc-erlang.png?branch=rewrite)](https://travis-ci.org/potatosalad/msgpack-rpc-erlang)

The [MessagePack RPC Specification](http://wiki.msgpack.org/display/MSGPACK/RPC+specification) client/server suite for [Erlang](http://erlang.org/) >= R15B0x.

msgpack_rpc_client
------------------
* [Connection](#connection)
  - [connect/3](#connect3)
  - [close/1](#close1)
* [Request](#request)
  - [call/3](#call3) [call/4](#call4)
  - [call_async/3](#call_async3) [call_async/4](#call_async4)
  - [join/2](#join2) [join/3](#join3)
* [Notify](#notify)
  - [notify/3](#notify3)
* Other
  - new/3
  - get/2
  - start_fsm/1
  - reset_stats/1
  - [stats/1](#stats1)

### Connection

#### connect/3

Takes `Address`, `Port`, and `Opts` as arguments and will return `{ok, #msgpack_rpc_client{}}` or `{error, Reason}`.

```erlang
%% TCP connection
{ok, Client} = msgpack_rpc_client:connect(localhost, 9199, []).

%% SSL connection
{ok, Client} = msgpack_rpc_client:connect(localhost, 9199,
    [{transport, ssl},
     {transport_opts, [{cert, Cert}, {key, Key}]}]).

%% Client-side caching (see available options below)
{ok, Client} = msgpack_rpc_client:connect(localhost, 9199, [{cache, true}]).

%% No reconnection attempts
{ok, Client} = msgpack_rpc_client:connect(localhost, 9199, [{reconnect, false}]).

%% Use msgpack_nif for packing and unpacking (see common options below)
{ok, Client} = msgpack_rpc_client:connect(localhost, 9199,
    [{msgpack_packer, fun msgpack_nif:pack/1},
     {msgpack_unpacker, fun msgpack_nif:unpack_stream/1}]).
```

##### Options

The following options can be passed to `connect/3`.

The format is `Option = Default :: Type | ...`

See also the [Common Options](#common-options) shared with the server.

* `timeout = 10000 :: timeout()`
    - `timeout()` the default timeout passed to `call/3`, `call_async/3`, and `join/2`
* `debug = false :: boolean()`
    - `true` logs reconnection attempts to `error_logger`
* `stats = off :: off | on | all`
    - `off` will not record any stats
    - `on` will record `request` and `notify` level stats, ignore method level stats
    - `all` will record method level stats
* Client-side caching
    * `cache = false :: boolean()`
        - `false` will cause `request` and `notify` messages to failfast with `{error, no_connection}`
        - `true` will attempt to use client-side caching of messages until a connection is available
    * `cache_blocks = true :: boolean()`
        - `false` will process incoming messages before emptying the cache queue
        - `true` will block incoming messages until the cache queue is empty
    * `cache_capped = true :: boolean()`
        - `false` will failfast with `{error, no_connection}` once `cache_limit` has been reached
        - `true` will drop the oldest messages from the cache queue once `cache_limit` has been reached
    * `cache_limit = infinity :: infinity | non_neg_integer()`
        - `infinity` will add messages to the cache queue indefinitely
        - `non_neg_integer()` will cause the client to failfast, stop, or drop old messages (depending on `cache_capped` and `cache_stops`)
    * `cache_stops = false :: boolean()`
        - `false` will not close the client when `cache_limit` is reached
        - `true` will close the client when `cache_limit` is reached
* Reconnecting
    * `reconnect = true :: boolean()`
        - `false` will close the client when the connection is lost
        - `true` will attempt to reconnect when the connection is lost
    * `reconnect_limit = infinity :: infinity | non_neg_integer()`
        - `infinity` will try reconnecting indefinitely
        - `non_neg_integer()` will close the client once `reconnect_limit` is reached
    * `reconnect_min = 100 :: timeout()`
        - `timeout()` the starting wait time for reconnection attempts.  This time will be doubled until `reconnect_max` is reached.
    * `reconnect_max = 5000 :: timeout()`
        - `timeout()` the maximum amount of time to wait between reconnection attempts.
* Transport
    * `transport = tcp :: tcp | ssl | module()`
        - `tcp` and `ssl` will be translated to `ranch_tcp` and `ranch_ssl` (which are also valid options).
    * `transport_opts = [] :: [proplists:property()]`
        - Passed to the `transport` like this: `Transport:connect(Address, Port, TransportOpts)`

#### close/1

Closes (or stops) the `msgpack_rpc_client_fsm` for the client.

```erlang
ok = msgpack_rpc_client:close(Client).
```

### Request

#### call/3

Internally performs the equivalent of a `call_async/3` followed by a `join/2`.
Uses the `timeout` option for the client.

```erlang
{ok, 123} = msgpack_rpc_client:call(echo, [123], Client).
```

#### call/4

Same as `call/3` with an additional `Timeout` argument.

```erlang
%% Wait forever for a response, or until connection dies.
{ok, 123} = msgpack_rpc_client:call(echo, [123], infinity, Client).
```

#### call_async/3

Sends the `request` to the server, returns `{ok, #msgpack_rpc_request{}}` or `{error, Reason}`.
In order to retrieve the response, `join/2` or `join/3` should be used.
Uses the `timeout` option for the client.

```erlang
{ok, Request} = msgpack_rpc_client:call_async(echo, [123], Client),
MsgId = msgpack_rpc_request:get(msg_id, Request).
```

#### call_async/4

Same as `call_async/3` with an additional `Timeout` argument.

```erlang
%% Wait forever until the request can be sent.
{ok, Request} = msgpack_rpc_client:call_async(echo, [123], infinity, Client).
```

#### join/2

Waits for a `response` from the server for the supplied `Request` as returned from `call_async/3`.
Returns `{ok, #msgpack_rpc_response{}}` or `{error, Reason}`.
Uses the `timeout` option for the client.

```erlang
{ok, 123} = msgpack_rpc_client:join(Request, Client).
```

#### join/3

Same as `join/2` with an additional `Timeout` argument.

```erlang
%% Wait forever for a response, or until the connection dies.
{ok, 123} = msgpack_rpc_client:join(Request, infinity, Client).
```

### Notify

#### notify/3

Sends the `notify` to the server, returns `ok`.

```erlang
ok = msgpack_rpc_client:notify(echo, [123], Client).
```

### Other

#### stats/1

Returns the stats for the client as configured by the `stats` option.

```erlang
%% {stats, off}
{ok, Client} = msgpack_rpc_client:connect(localhost, 9199, [{stats, off}]),
{ok, 123} = msgpack_rpc_client:call(echo, [123], Client),
msgpack_rpc_client:stats(Client).

{ok,[]}

%% {stats, on}
{ok, Client} = msgpack_rpc_client:connect(localhost, 9199, [{stats, on}]),
{ok, 123} = msgpack_rpc_client:call(echo, [123], Client),
msgpack_rpc_client:stats(Client).

{ok,[{<<"request_send">>,1,6341},
     {<<"request_response">>,1,2198}]}

%% {stats, all}
{ok, Client} = msgpack_rpc_client:connect(localhost, 9199, [{stats, all}]),
{ok, 123} = msgpack_rpc_client:call(echo, [123], Client),
msgpack_rpc_client:stats(Client).

{ok,[{<<"request_send">>,1,3570},
     {<<"request_response">>,1,8802},
     {<<"echo_request_send">>,1,3570},
     {<<"echo_request_response">>,1,8802}]}
```
