-module(pingpong).

-export([run/1, bench/3, pingpong_player/1, iota/1]).


iota(0) -> [];
iota(N) -> [N] ++ iota(N - 1).

pingpong_player(N) ->
    receive
	{From, done} ->
	    From ! N,
	    exit(normal);
	{From, Ball} ->
	    From ! {self(), Ball}
    end,
    pingpong_player(N + 1).

bench(Duration, Len, Remote) ->
    Player1 = spawn(pingpong, pingpong_player, [0]),
    Player2 = spawn(Remote, pingpong, pingpong_player, [1]),
    Player1 ! {Player2, iota(Len)},
    receive
	after Duration -> ok
    end,
    Player1 ! {self(), done},
    Player2 ! {self(), done},
    receive
	X -> 
	    io:format("(pingpong erlang ~w ~w)~n",
		      [Len, round(X / (Duration / 1000))])
    end.

run([Len, Node]) ->
    Remote = case Node of 
		 "remote" -> 
		     receive X -> X end;
		 _ -> list_to_atom(Node) 
	     end,
    Duration = 5000,
    bench(Duration, list_to_integer(Len), Remote),
    spawn(Remote, erlang, halt, [0]),
    halt(0).
