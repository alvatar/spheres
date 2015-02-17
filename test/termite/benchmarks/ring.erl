-module(ring).

-export([run/1, ring/2, make_relay/1]).

make_relay(Next) ->
    receive 
	K when K > 0 ->
	    Next ! K - 1,
	    make_relay(Next);
	
	K ->
	    Next ! K
    end.

loop(K, Current, N) when N > 1 ->
    loop(K,
	 spawn(ring, make_relay, [Current]),
	 N - 1);

loop(K, Current, _) ->
    self() ! K,
    make_relay(Current).

ring(N, K) ->
    loop(K, self(), N).

run([N, K]) ->
    N1 = list_to_integer(N),
    K1 = list_to_integer(K),
    {Time, _} = timer:tc(ring,ring,[N1,K1]),
    io:format("(ring erlang (~w ~w) ~w)~n", [N1, K1, round(Time / 1000)]),
    halt(0).
