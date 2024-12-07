% Advent of Code 2024, Day 7 - Bridge Repair
% by Dan Higdon

% parse the data
read_data_line(F,(Gi,Ri)) :-
   read_line_to_string(F,S),
   split_string(S,':',' ',[G,Rest]),
   number_string(Gi,G),
   split_string(Rest,' ',' ',R),
   maplist(number_string,Ri,R).

read_all_lines(F,L,[V|E]) :-
   read_data_line(F,V),
   read_all_lines(F,L,E).
read_all_lines(_,L,L).

read_data_file(Fname,Lines) :-
   open(Fname,read,F),
   read_all_lines(F,[],Lines),
   close(F),!. % once we close the file, we're done

% utility to concat strings of numbers
combine(X,Y,Z) :-
   number_string(X,Xs),
   number_string(Y,Ys),
   string_concat(Xs,Ys,Zs),
   number_string(Z,Zs).

% Goal to solve the problem
goal(T,T,[]).
goal(T,V,[X|Rest]) :- N is V + X,     goal(T,N,Rest).
goal(T,V,[X|Rest]) :- N is V * X,     goal(T,N,Rest).
goal(T,V,[X|Rest]) :- combine(V,X,N), goal(T,N,Rest).

part1(Fname,Result) :-
   read_data_file(Fname,Lines),
   findall(G,
           (member((G,L),Lines),
            once(goal(G,0,L))),
           Rs),
   sumlist(Rs,Result).

