-module(qsort).

-export([run/1, qsort/1]).

qsort(L) ->
   qsort(L, []).

qsort([X|L], ReallyBigs) ->
   {S, B} = partition(L, X),
   SB = qsort(B, ReallyBigs),
   qsort(S, [X|SB]);
qsort([], Bigs) ->
   Bigs.


partition([X|Rest], Y) when X =< Y ->
   {S, B} = partition(Rest, Y),
   {[X|S], B};
partition([X|Rest], Y) ->
   {S, B} = partition(Rest, Y),
   {S, [X|B]};
partition([],_) -> 
   {[], []}.


mkrandlist(X) ->
   random:seed(),
   mkrandlist(X, []).

mkrandlist(0, L) ->
   L;
mkrandlist(X, L) ->
   mkrandlist(X-1, [random:uniform(1000000) | L]).


run([Arg]) ->
    N = list_to_integer(Arg),
    L = mkrandlist(N),
    {Time, _} = timer:tc(qsort,qsort,[L]),
    io:format("(qsort erlang ~w ~w)~n", [N, round(Time / 1000)]),
    halt(0).
