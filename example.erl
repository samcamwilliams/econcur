-module(example).
-export([test/1, tc/1]).
-compile({parse_transform, econcur}).

test(T) ->
	X = long_process(T),
	Y = long_process(T + 1),
	Z = long_process(X),
	lists:sum([X, Y, Z]).

long_process(T) ->
	timer:sleep(T * 1000),
	T + 1.

% tc(1) completes in ~3 seconds, rather than the normal 5.
tc(X) -> timer:tc(fun() -> test(X) end).
