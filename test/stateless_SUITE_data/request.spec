{custom_error, [], {error, unknown}}.
{echo, ["test"], {ok, "test"}}.
{function_clause, ["ok"], {ok, "ok"}}.
{function_clause, ["error"], {error, function_clause}}.
{ping, [], {ok, "pong"}}.
{sum, [1, 2], {ok, 3}}.
{sum, [1], {error, undef}}.
{undefined, [], {error, undef}}.