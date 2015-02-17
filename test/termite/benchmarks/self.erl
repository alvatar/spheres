-module(self).

-export([run/1, test/1]).

test(0) -> ok;

test(N) -> 
    self() ! (N - 1),
    receive N1 -> test(N1) end.

run([Arg]) ->
    N = list_to_integer(Arg),
    {Time, _} = timer:tc(self, test, [N]),
    io:format("(self erlang ~w ~w)~n", [N, round(Time / 1000)]),
    halt(0).

