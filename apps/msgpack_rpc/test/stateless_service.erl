-module(stateless_service).

-export([custom_error/0,
         echo/1,
         echo/2,
         function_clause/1,
         ping/0,
         ping/1,
         sum/2,
         sum/3]).

custom_error() ->
    erlang:error(custom_error).

echo(X) ->
    X.

echo(RawFrom, X) ->
    From = binary_to_term(RawFrom),
    From ! X,
    X.

function_clause("ok") ->
    "ok".

ping() ->
    "pong".

ping(RawFrom) ->
    From = binary_to_term(RawFrom),
    From ! "pong",
    "pong".

sum(X, Y) ->
    X + Y.

sum(RawFrom, X, Y) ->
    Z = X + Y,
    From = binary_to_term(RawFrom),
    From ! Z,
    Z.
