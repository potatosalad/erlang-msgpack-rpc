%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  31 May 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------

-module(msgpack_rpc_notify_fsm_sup).
-behaviour(supervisor).

%% API
-export([start/1,
         start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-ignore_xref([init/1,
              start_link/0]).

%%====================================================================
%% API functions
%%====================================================================

start(Args) ->
    supervisor:start_child(?MODULE, Args).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    NotifyFsm = {undefined, {msgpack_rpc_notify_fsm, start_link, []},
        temporary, 5000, worker, [msgpack_rpc_notify_fsm]},
    {ok, {{simple_one_for_one, 10, 10}, [NotifyFsm]}}.
