%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Program     : Smith Waterman string matching algorithm Erlang version       %
% File        : smith.erl                                                     %
% Author      : Alexander Jean-Claude Bottema (alexb@csd.uu.se)               %
% Datum       : Sep 19 1995                                                   %
% Revidated   : Sep 19 1995                                                   %
% --------------------------------------------------------------------------- %
% Changes:                                                                    %
%    Optimization 1: Inline "match_weights"                                   %
%    Optimization 2: Guards on all int variables (explicit type information)  %
% --------------------------------------------------------------------------- %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(smith).
-export([run/1, test/1, generate_sequence/2, match_sequences/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% max(A,B) - The maximum value of A and B
%

max(A,B) when integer(A), integer(B) ->
    if A > B -> A
     ; true  -> B
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% alpha_beta_penalty(A,B) - The penalty value of A and B
%
alpha_beta_penalty(A,B) when integer(A), integer(B) -> max(A-4,B-1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% match_weights(A,B) - Weight function of A and B
%
% match_weights(A,B) -> if A == B -> 1 ; true -> 0 end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% generate_sequence( Length, Seed ) - Generate a random sequence of length
%                                     'Length' using seed value 'Seed'.
%

generate_sequence( Length, R ) when integer(Length) ->
    if  Length == 0 -> []
      ; true        ->  [R rem 10 | generate_sequence( Length - 1,
						      (R * 11 + 1237501)
						      rem 10067)]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% generate_sequences( No, Length, Seed )
%

generate_sequences( 0, _, _ ) -> [] ;
generate_sequences( N, Length, R ) when integer(N), integer(Length) ->
    [generate_sequence(Length, R) | generate_sequences(N-1,Length,R+1)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Entry representation:
%
% Entry = {Left,Upper,UpperLeft,Max}
%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% match_entry(Top,Side,UpperLeft,Upper,Left) -
%     Match sequence entries with surrounding information

match_entry(Top,Side,UpperLeft,Upper,Left) when integer(Top), integer(Side) ->
    MeLeft = alpha_beta_penalty( element( 3, Left ), element( 1, Left ) ),
    MeUpper = alpha_beta_penalty( element( 3, Upper ), element( 2, Upper ) ),
    %
    % match weight removed
    %
    if Top == Side ->
	    MeUpperLeft = 
		max(MeLeft,
		    max(MeUpper,
			max( element( 3, UpperLeft ) + 1, 0 )))
		;   
       true -> 
	    MeUpperLeft =
		max(MeLeft,
		    max(MeUpper,
			max( element( 3, UpperLeft ), 0 )))
    end,
    {MeLeft, MeUpper, MeUpperLeft,
     max(MeUpperLeft,
	  max(element(4, Left),
	       max(element(4, Upper), element(4, UpperLeft))))}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% match_zero_entry( Top, Side, Left )
%

match_zero_entry( Top, Side, {Left,_,UpperLeft,Max} ) when integer(Top), integer(Side) ->
    ELeft = alpha_beta_penalty(UpperLeft, Left),
    %Weight = max(1-abs(Side-Top),0),
    EUpperLeft = max(max(ELeft,max(1-abs(Side-Top),0)),0),
    EMax = max(max(Max,EUpperLeft),0),
    {ELeft, -1, EUpperLeft, EMax}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% match(Top, Side, Prev, UpperLeft, Left )
%

match(Tops, Side, Prev, UpperLeft, Left) ->
    match0(Tops, Side, Prev, UpperLeft, Left, [], none).

match0([], _, _, _, _, Acc, Last) -> {Acc,Last} ;
match0([Top|Tops], Side, [Upper|Prev], UpperLeft, Left, Acc, _) when
    integer(Top), integer(Side) ->
    E = match_entry(Top, Side, UpperLeft, Upper, Left),
    match0(Tops, Side, Prev, Upper, E, [E|Acc], E) ;
match0([Top|Tops], Side, none, UpperLeft, Left, Acc, _) when
    integer(Top), integer(Side) ->
    E = match_zero_entry(Top, Side, Left ),
    match0(Tops, Side, none, UpperLeft, E, [E|Acc], E).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% match_two_seq(Side, Top, Prev)
%

match_two_seq(Side, Top, Prev) ->
    match_two_seq0(Side, Top, Prev, none).

match_two_seq0([], _, _, Result) -> Result ;
match_two_seq0([S|Side], Top, Prev, _) when integer(S) ->
    {Row,Result} = match(Top,S,Prev,{0,0,0,0},{0,0,0,0}),
    match_two_seq0(Side, Top, Row, Result).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% match_sequences(Tops, Side)
%

match_sequences(Tops, Side) ->
    match_sequences0(Tops, Side, -9999999).

match_sequences0([], _, MaxResult) -> MaxResult ;
match_sequences0([Top|Tops], Side, CrntResult) ->
    Result = element(4, match_two_seq(Top, Side, none)),
    match_sequences0(Tops, Side, max(CrntResult, Result)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test(N)
%

test(N) ->
    Tops = generate_sequences(N, 32, 1),
    Side = generate_sequence(32, 0),
    {Time, _} = timer:tc(smith, match_sequences, [Tops, Side]),
    io:format("(smith erlang ~w ~w)~n", [N, round(Time / 1000)]).

run([Arg]) -> 
    N = list_to_integer(Arg),
    test(N),
    halt(0).
