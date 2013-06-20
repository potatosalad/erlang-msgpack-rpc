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
-export([new/1, get/2]).

%% Internal
-import(msgpack_rpc_util, [get_value/3]).

-type obj() :: #msgpack_rpc_options{}.
-export_type([obj/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new([proplists:property()]) -> obj().
new(Opts) ->
    new_(merge([
        error_decoder,
        error_encoder,
        msgpack_packer,
        msgpack_unpacker], Opts, #msgpack_rpc_options{})).

-spec get(list(atom()) | atom(), obj()) -> list(term()) | term().
get(List, Req) when is_list(List) ->
    [g(Atom, Req) || Atom <- List];
get(Atom, Req) when is_atom(Atom) ->
    g(Atom, Req).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
-spec new_([term()]) -> obj().
new_([ErrorDecoder, ErrorEncoder, MsgpackPacker, MsgpackUnpacker]) ->
    #msgpack_rpc_options{error_decoder=ErrorDecoder,
        error_encoder=ErrorEncoder, msgpack_packer=MsgpackPacker,
        msgpack_unpacker=MsgpackUnpacker}.

%% @private
g(error_decoder, #msgpack_rpc_options{error_decoder=Ret}) -> Ret;
g(error_encoder, #msgpack_rpc_options{error_encoder=Ret}) -> Ret;
g(msgpack_packer, #msgpack_rpc_options{msgpack_packer=Ret}) -> Ret;
g(msgpack_unpacker, #msgpack_rpc_options{msgpack_unpacker=Ret}) -> Ret.

%% @private
merge(List, Opts, Obj) when is_list(List) ->
    [merge(Atom, Opts, Obj) || Atom <- List];
merge(Atom, Opts, Obj) when is_atom(Atom) ->
    get_value(Atom, Opts, g(Atom, Obj)).
