-module(spawn).

-export([run/1, test/1]).

test(0) -> ok;

test(N) -> 
    F = fun () -> N end,
    spawn(F),
    test(N - 1).

run([Arg]) ->
    N = list_to_integer(Arg),
    {Time, _} = timer:tc(spawn, test, [N]),
    io:format("(spawn erlang ~w ~w)~n", [N, round(Time / 1000)]),
    halt(0).

