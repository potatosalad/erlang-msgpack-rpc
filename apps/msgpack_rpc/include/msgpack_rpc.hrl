%%
%% MessagePack for Erlang
%%
%% Copyright (C) 2010 UENISHI Kota
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

-define(MSGPACK_RPC_REQUEST,  0).
-define(MSGPACK_RPC_RESPONSE, 1).
-define(MSGPACK_RPC_NOTIFY,   2).

-record(msgpack_rpc_request, {
    msg_id = undefined :: undefined | msgpack_rpc:msg_id(),
    method = undefined :: undefined | msgpack_rpc:method(),
    params = undefined :: undefined | msgpack_rpc:params()
}).

-record(msgpack_rpc_response, {
    %% Request.
    request = undefined :: undefined | msgpack_rpc:request(),

    %% Response.
    msg_id = undefined :: undefined | msgpack_rpc:msg_id(),
    error  = undefined :: undefined | msgpack_rpc:error(),
    result = undefined :: undefined | msgpack_rpc:result()
}).

-record(msgpack_rpc_notify, {
    method = undefined :: undefined | msgpack_rpc:method(),
    params = undefined :: undefined | msgpack_rpc:params()
}).

-record(msgpack_rpc_options, {
    %% Errors
    error_decoder = fun msgpack_rpc_util:binary_to_known_error/1 :: function(),
    error_encoder = fun msgpack_rpc_util:known_error_to_binary/1 :: function(),

    %% msgpack
    msgpack_packer   = fun msgpack:pack/1          :: function(),
    msgpack_unpacker = fun msgpack:unpack_stream/1 :: function()
}).
