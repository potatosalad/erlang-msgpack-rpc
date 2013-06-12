-module(stateless_service).

-export([echo/1]).
-export([echo/2]).
-export([ping/0]).
-export([ping/1]).
-export([sum/2]).
-export([sum/3]).

echo(X) ->
    X.

echo(RawFrom, X) ->
    From = binary_to_term(RawFrom),
    From ! X,
    X.

ping() ->
    <<"pong">>.

ping(RawFrom) ->
    From = binary_to_term(RawFrom),
    From ! pong,
    <<"pong">>.

sum(X, Y) ->
    X + Y.

sum(RawFrom, X, Y) ->
    Z = X + Y,
    From = binary_to_term(RawFrom),
    From ! Z,
    Z.
