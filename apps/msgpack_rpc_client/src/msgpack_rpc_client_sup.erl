%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  17 June 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------

-module(msgpack_rpc_client_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-ignore_xref([init/1,
              start_link/0]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init(_Args) ->
    ClientFSMs = {msgpack_rpc_client_fsm_sup,
        {msgpack_rpc_client_fsm_sup, start_link, []},
        permanent, infinity, supervisor, [msgpack_rpc_client_fsm_sup]},
    {ok, {{one_for_one, 5, 10}, [ClientFSMs]}}.
