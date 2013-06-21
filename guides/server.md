Server
======
* msgpack_rpc_server
    - [start_listener/5](#start_listener5)
    - [start_service/5](#start_service5)
    - [stop_listener/1](#stop_listener1)
* msgpack_rpc_handler
    - [msgpack_rpc_init/2](#msgpack_rpc_init2)
    - [msgpack_rpc_handle/3](#msgpack_rpc_handle3)
    - [msgpack_rpc_info/2](#msgpack_rpc_info2)
    - [msgpack_rpc_terminate/3](#msgpack_rpc_terminate3)

msgpack_rpc_server
------------------

### start_listener/5

Start a `msgpack_rpc_protocol` listener for the given transport.
Accepts `Ref`, `NbAcceptors`, `Transport`, `TransOpts`, and `ProtoOpts`.

Depending on the type of `dispatcher` passed with `ProtoOpts`, other options may be required.
For the default `msgpack_rpc_dispatcher` the option `handler` is always required.
Other possible `ProtoOpts` are defined below.

```erlang
%% TCP listener
ok = application:start(crypto),
ok = application:start(ranch),
{ok, Pid} = msgpack_rpc_server:start_listener(my_handler_server, 4, ranch_tcp,
    [{port, 9199}],
    [{handler, my_handler}]).

%% SSL listener
ok = application:start(crypto),
ok = application:start(ssl),
ok = application:start(ranch),
{ok, Pid} = msgpack_rpc_server:start_listener(my_handler_server, 4, ranch_ssl,
    [{port, 9199}, {cert, Cert}, {key, Key}],
    [{handler, my_handler}]).
```

#### Options

The following options can be passed to `start_listener/5`.

The format is `Option = Default :: Type | ...`

See also the [Common Options](common-options.md) shared with the server.

* `dispatcher = msgpack_rpc_dispatcher :: module()`
    - `module()` implements the behavior callbacks of `msgpack_rpc_dispatcher`.
* `handler = undefined :: module()`
    - `module()` implements the behavior callbacks of `msgpack_rpc_handler`
* `handler_opts = [] :: any()`
    - Passed to `handler` in the form of `Handler:msgpack_rpc_init({Transport, msgpack_rpc}, HandlerOpts)`
* `timeout = infinity :: infinity | timeout()`
    - `infinity` run server indefinitely
    - `timeout()` shutdown server after not receiving any messages within `timeout`

### start_service/5

Internally calls `start_listener/5` with the `handler` set to `msgpack_rpc_stateless`.

Here is an example service named `my_service`:

```erlang
-module(my_service).
-export([echo/1,
         sum/2]).

echo(Message) ->
    Message.

sum(X, Y) ->
    X + Y.
```

The corresponding `start_service/5` call:

```erlang
ok = application:start(crypto),
ok = application:start(ranch),
{ok, Pid} = msgpack_rpc_server:start_service(my_service_server, 4, ranch_tcp, my_service, [{port, 9199}]).
```

And client usage:

```erlang
{ok, Client} = msgpack_rpc_client:connect(localhost, 9199, []),
{ok, 3} = msgpack_rpc_client:call(sum, [1, 2], Client).
```

### stop_listener/1

Stops a listener based on `Ref`.

```erlang
ok = msgpack_rpc_server:stop_listener(my_service_server).
```

msgpack_rpc_handler
-------------------

This module is used by the default `msgpack_rpc_dispatcher`, please see the module for actual callback specifications.

Here is an example module named `my_handler`, for a more complete example, see `msgpack_rpc_stateless`.

```erlang
-module(my_handler).
-behaviour(msgpack_rpc_handler).
-export([msgpack_rpc_init/2,
         msgpack_rpc_handle/3,
         msgpack_rpc_info/2,
         msgpack_rpc_terminate/3]).

msgpack_rpc_init({_, msgpack_rpc}, _Opts) ->
    {ok, undefined}.

msgpack_rpc_handle({request, <<"echo">>, [Message]}, Task, State) ->
    {respond, {result, Message}, Task, State};
msgpack_rpc_handle({request, _Method, _Params}, Task, State) ->
    {respond, {error, undef}, Task, State};
msgpack_rpc_handle({notify, _Method, _Params}, Task, State) ->
    {ignore, Task, State}.

msgpack_rpc_info(_Info, State) ->
    {shutdown, {error, badinfo}, State}.

msgpack_rpc_terminate(_Reason, _Task, _State) ->
    ok.
```

### msgpack_rpc_init/2

Called when a connection has been accepted by the server, passes `{Transport:name(), msgpack_rpc}, HandlerOpts`.

### msgpack_rpc_handle/3

Accepts `{request, Method, Params}` or `{notify, Method, Params}` as the first argument with `Task=#msgpack_rpc_task{}` and `State` as the other two.

### msgpack_rpc_info/2

Any other messages received are handled here.

### msgpack_rpc_terminate/3

`Task` will be the task that caused the shutdown or `undefined` if it was initiated some other way.
