%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  31 May 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------

-module(msgpack_rpc_handler).

-callback msgpack_rpc_init({atom(), msgpack_rpc}, HandlerOpts::any())
    -> {ok, State}
    | {ok, State, hibernate}
    | {ok, State, timeout()}
    | {ok, State, timeout(), hibernate}
    | {shutdown, Reason::any(), State}.
-callback msgpack_rpc_handle(Req::msgpack_rpc_task:message(), Task::msgpack_rpc:task(), State::any())
    -> {ok, Task, State}
    | {ok, Task, State, hibernate}
    | {ok, Task, State, timeout()}
    | {ok, Task, State, timeout(), hibernate}
    | {execute, msgpack_rpc_task:job(), Task, State}
    | {execute, msgpack_rpc_task:job(), Task, State, hibernate}
    | {execute, msgpack_rpc_task:job(), Task, State, timeout()}
    | {execute, msgpack_rpc_task:job(), Task, State, timeout(), hibernate}
    | {ignore, Task, State}
    | {ignore, Task, State, hibernate}
    | {ignore, Task, State, timeout()}
    | {ignore, Task, State, timeout(), hibernate}
    | {respond, msgpack_rpc_task:response(), Task, State}
    | {respond, msgpack_rpc_task:response(), Task, State, hibernate}
    | {respond, msgpack_rpc_task:response(), Task, State, timeout()}
    | {respond, msgpack_rpc_task:response(), Task, State, timeout(), hibernate}
    | {shutdown, Reason::any(), Task, State}.
-callback msgpack_rpc_info(Info::term(), State::any())
    -> {ok, State}
    | {ok, State, hibernate}
    | {ok, State, timeout()}
    | {ok, State, timeout(), hibernate}
    | {shutdown, Reason::any(), State}.
-callback msgpack_rpc_terminate(Reason::term(), Task :: undefined | msgpack_rpc:task(), State::any())
    -> term().
