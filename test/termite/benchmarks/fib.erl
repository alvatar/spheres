-module(fib).

-export([run/1, fib/1]).


fib(X) when X < 2 -> X;
fib(X) ->
    fib(X-1)+fib(X-2).

run([Arg]) ->
    N = list_to_integer(Arg),
    {Time, _} = timer:tc(fib,fib,[N]),
    io:format("(fib erlang ~w ~w)~n", [N, round(Time / 1000)]),
    halt(0).
