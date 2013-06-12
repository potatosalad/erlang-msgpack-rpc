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

-ifndef(MP_RPC_HRL).

-define(MP_TYPE_REQUEST,  0).
-define(MP_TYPE_RESPONSE, 1).
-define(MP_TYPE_NOTIFY, 2).

-type name() :: atom().
-type global_name() :: term().

-type transport() :: tcp | udp. % sctp | snappy | zip | etc...
-type nport() :: (1..65535).
 
-record(mprc, { s :: inet:socket(),
		carry = <<>> :: binary(),
		transport = tcp :: transport(),
		transport_mod = mprc_tcp :: atom(),
		host :: inet:ip_address(),
		port :: nport()
	      }).
-type mprc() :: #mprc{}.

-type server_name() :: {local, name()} | {global, global_name()}.
-type server_ref() :: pid() | name() | { name(), node() } | {global, global_name()}.

-endif.
