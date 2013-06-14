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
    | {shutdown, State}.
-callback msgpack_rpc_handle(Req::msgpack_rpc_task:req(), Task::msgpack_rpc:task(), State::any())
    -> {ok, Task, State}
    | {ok, Task, State, hibernate}
    | {ok, Task, State, timeout()}
    | {ok, Task, State, timeout(), hibernate}
    | {shutdown, Task, State}.
-callback msgpack_rpc_info(Info::term(), State::any())
    -> {ok, State}
    | {ok, State, hibernate}
    | {ok, State, timeout()}
    | {ok, State, timeout(), hibernate}
    | {shutdown, State}.
-callback msgpack_rpc_terminate(Reason::term(), Task :: undefined | msgpack_rpc:task(), State::any())
    -> term().
