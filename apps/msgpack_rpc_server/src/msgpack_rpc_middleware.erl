%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  31 May 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------

-module(msgpack_rpc_middleware).

-type env() :: [{atom(), any()}].
-export_type([env/0]).

-callback init({atom(), msgpack_rpc}, Env::env())
    -> {ok, Env}
    | {ok, Env, hibernate}
    | {ok, Env, timeout()}
    | {ok, Env, timeout(), hibernate}
    | {shutdown, Env}.
-callback request(Req::msgpack_rpc_request:req(), Env::env())
    -> {ok, Req, Env}
    | {ok, Req, Env, hibernate}
    | {ok, Req, Env, timeout()}
    | {ok, Req, Env, timeout(), hibernate}
    | {shutdown, Req, Env}.
-callback notify(Req::msgpack_rpc_notify:req(), Env::env())
    -> {ok, Req, Env}
    | {ok, Req, Env, hibernate}
    | {ok, Req, Env, timeout()}
    | {ok, Req, Env, timeout(), hibernate}
    | {shutdown, Req, Env}.
-callback info(Info::term(), Env::env())
    -> {ok, Env}
    | {ok, Env, hibernate}
    | {ok, Env, timeout()}
    | {ok, Env, timeout(), hibernate}
    | {shutdown, Env}.
-callback terminate(Reason::term(), Req :: undefined | msgpack_rpc_notify:req() | msgpack_rpc_request:req(), Env::env())
    -> term().

-export([init/2,
         request/2,
         notify/2,
         info/2,
         terminate/3]).

-export([get_state/2,
         set_state/3]).

init(Transport, Env) ->
    io:format("[init] ~p ~p~n", [Transport, Env]),
    {ok, Env}.

request(Req, Env) ->
    io:format("[request] ~p ~p~n", [Req, Env]),
    {ok, Req, Env}.

notify(Req, Env) ->
    io:format("[notify] ~p ~p~n", [Req, Env]),
    {ok, Req, Env}.

info(Info, Env) ->
    io:format("[info] ~p ~p~n", [Info, Env]),
    {ok, Env}.

terminate(Reason, Req, Env) ->
    io:format("[terminate] ~p ~p ~p~n", [Reason, Req, Env]),
    ok.

get_state(Key, Env) ->
    try lists:keyfind(Key, 1, Env) of
        {_, State} ->
            State
    catch _:_ ->
        undefined
    end.

set_state(Key, State, Env) ->
    lists:keystore(Key, 1, Env, {Key, State}).
