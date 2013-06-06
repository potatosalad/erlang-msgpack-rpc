-include_lib("msgpack_rpc/include/msgpack_rpc.hrl").

-record(msgpack_rpc_notify, {
    %% Notify.
    method = undefined :: undefined | binary(),
    params = undefined :: undefined | any(),

    %% FSM.
    fsm_ref  = undefined :: undefined | any(),
    executed = false     :: boolean()
}).

-record(msgpack_rpc_request, {
    %% Request.
    msg_id = undefined :: undefined | non_neg_integer(),
    method = undefined :: undefined | binary(),
    params = undefined :: undefined | any(),

    %% FSM.
    fsm_ref  = undefined :: undefined | any(),
    executed = false     :: boolean(),

    %% Response.
    socket       = undefined :: undefined | inet:socket(),
    transport    = undefined :: undefined | module(),
    msgpack_opts = undefined :: undefined | any()
}).
