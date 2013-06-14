%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  11 June 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(msgpack_rpc_options).

-include("msgpack_rpc.hrl").

%% API
-export([new/1,
         from_options/1,
         get/2]).

-type obj() :: #msgpack_rpc_options{}.
-export_type([obj/0]).

-define(OPTIONS, [error_decoder, error_encoder, msgpack_packer, msgpack_unpacker]).

%%%===================================================================
%%% API
%%%===================================================================

% -spec new([function(), function(), function(), function(), [proplists:property()]])
%     -> {ok, obj()}.
new([ErrorDecoder, ErrorEncoder, MsgpackPacker, MsgpackUnpacker]) ->
    Options = #msgpack_rpc_options{error_decoder=ErrorDecoder,
        error_encoder=ErrorEncoder, msgpack_packer=MsgpackPacker,
        msgpack_unpacker=MsgpackUnpacker},
    {ok, Options}.

-spec from_options([proplists:property()]) -> {ok, obj()}.
from_options(Opts) ->
    new(get(?OPTIONS, Opts, #msgpack_rpc_options{})).

%% @private
get(List, Req) when is_list(List) ->
    [g(Atom, Req) || Atom <- List];
get(Atom, Req) when is_atom(Atom) ->
    g(Atom, Req).

g(error_decoder, #msgpack_rpc_options{error_decoder=Ret}) -> Ret;
g(error_encoder, #msgpack_rpc_options{error_encoder=Ret}) -> Ret;
g(msgpack_packer, #msgpack_rpc_options{msgpack_packer=Ret}) -> Ret;
g(msgpack_unpacker, #msgpack_rpc_options{msgpack_unpacker=Ret}) -> Ret.

%% @private
get(List, Opts, Obj) when is_list(List) ->
    [get(Atom, Opts, Obj) || Atom <- List];
get(Atom, Opts, Obj) when is_atom(Atom) ->
    get_value(Atom, Opts, g(Atom, Obj)).

%% @doc Faster alternative to proplists:get_value/3.
%% @private
get_value(Key, Opts, Default) ->
  case lists:keyfind(Key, 1, Opts) of
    {_, Value} -> Value;
    _ -> Default
  end.
