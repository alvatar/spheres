-module(tak).

-export([run/1, tak/3]).

tak(X, Y, Z) when X =< Y ->
   Z;
tak(X, Y, Z) ->
   tak(tak(X-1, Y, Z), tak(Y-1, Z, X), tak(Z-1, X, Y)).


run([Arg1, Arg2, Arg3]) ->
    X = list_to_integer(Arg1),
    Y = list_to_integer(Arg2),
    Z = list_to_integer(Arg3),
    {Time, _} = timer:tc(tak,tak,[X, Y, Z]),
    io:format("(tak erlang (~w ~w ~w) ~w)~n", [X, Y, Z, round(Time / 1000)]),
    halt(0).
