%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  17 June 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------

%% @private
-module(msgpack_rpc_client_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
    msgpack_rpc_client_sup:start_link().

stop(_State) ->
    ok.
