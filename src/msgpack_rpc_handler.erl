%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  31 May 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------

-module(msgpack_rpc_handler).

-type req() :: {msgpack_rpc:msg_id(), msgpack_rpc:method(), msgpack_rpc:params()} | {msgpack_rpc:method(), msgpack_rpc:params()}.

-callback msgpack_rpc_init({atom(), msgpack_rpc}, HandlerOpts::any())
    -> {ok, State}
    | {ok, State, hibernate}
    | {ok, State, timeout()}
    | {ok, State, timeout(), hibernate}
    | {shutdown, Reason::any(), State}.
-callback msgpack_rpc_request({msgpack_rpc:msg_id(), msgpack_rpc:method(), msgpack_rpc:params()}, From::{pid(), msgpack_rpc:msg_id()}, State::any())
    -> {noreply, State}
    | {noreply, State, hibernate}
    | {noreply, State, timeout()}
    | {noreply, State, timeout(), hibernate}
    | {reply, msgpack_rpc:reply(), State}
    | {reply, msgpack_rpc:reply(), State, hibernate}
    | {reply, msgpack_rpc:reply(), State, timeout()}
    | {reply, msgpack_rpc:reply(), State, timeout(), hibernate}
    | {shutdown, Reason::any(), State}.
-callback msgpack_rpc_notify({msgpack_rpc:method(), msgpack_rpc:params()}, State::any())
    -> {noreply, State}
    | {noreply, State, hibernate}
    | {noreply, State, timeout()}
    | {noreply, State, timeout(), hibernate}
    | {reply, msgpack_rpc:reply(), State}
    | {reply, msgpack_rpc:reply(), State, hibernate}
    | {reply, msgpack_rpc:reply(), State, timeout()}
    | {reply, msgpack_rpc:reply(), State, timeout(), hibernate}
    | {shutdown, Reason::any(), State}.
-callback msgpack_rpc_info(Info::term(), State::any())
    -> {noreply, State}
    | {noreply, State, hibernate}
    | {noreply, State, timeout()}
    | {noreply, State, timeout(), hibernate}
    | {shutdown, Reason::any(), State}.
-callback msgpack_rpc_terminate(Reason::term(), Req::req(), State::any())
    -> term().
