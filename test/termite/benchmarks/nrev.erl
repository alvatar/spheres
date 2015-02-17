-module(nrev).

-export([run/1, nrev/1, iota/1]).


iota(X) ->
    iota (X-1, []).

iota(0, L) -> 
    [0|L];

iota(X, L) -> 
    iota(X-1, [X|L]).

nrev([]) ->
    [];

nrev([A|B]) ->
    nrev(B) ++ [A].


run([Arg]) ->
    N = list_to_integer(Arg),
    L = iota(N),
    {Time, _} = timer:tc(nrev,nrev,[L]),
    io:format("(nrev erlang ~w ~w)~n", [N, round(Time / 1000)]),
    halt(0).
