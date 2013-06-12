%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  31 May 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------

-module(msgpack_rpc_server_sup).
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
    TaskFSMs = {msgpack_rpc_task_fsm_sup,
        {msgpack_rpc_task_fsm_sup, start_link, []},
        permanent, infinity, supervisor, [msgpack_rpc_task_fsm_sup]},
    {ok, {{one_for_one, 5, 10}, [TaskFSMs]}}.
