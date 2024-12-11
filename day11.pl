% Advent of Code 2024, Day 11 - Plutonian Pebbles
% by Dan Higdon

% data
data(test,["125", "17"]).
data(main,["1", "24596", "0", "740994", "60", "803", "8918", "9405859"]).

% Utilities
even(N):- mod(N,2) =:= 0. 
reduce_num(S,V) :- number_string(N,S), number_string(N,V).

% blink rule
blink([],R,R) :- !.
blink(["0"|Rest],R,E) :- blink(Rest,["1"|R],E), !.
blink([Pebble|Rest],R,E) :-
   string_length(Pebble,L),
   even(L),
   Half is L / 2,
   sub_string(Pebble,0,Half,Half,L1), reduce_num(L1,Left),
   sub_string(Pebble,Half,Half,0,R1), reduce_num(R1,Right),
   !, blink(Rest,[Left,Right|R],E).
blink([Pebble|Rest],R,E) :-
   number_string(P,Pebble),
   N is P * 2024,
   number_string(N,New),
   !, blink(Rest,[New|R],E).

% iterated blinks
blinks(0,P,P) :- !.
blinks(N,P,R) :-
   blink(P,[],T),
   N1 is N - 1,!,
   blinks(N1,T,R).
