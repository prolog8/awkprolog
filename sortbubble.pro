sort(L, S) :- swap(L, L1), !, sort(L1, S).
sort(S, S).
swap([X, Y | R], [Y, X | R]) :- X > Y.
swap([Z | R], [Z | R1]) :- swap(R, R1).
 
?- sort([6, 45, 30, 21, 23, 20, 8, 7, 32, 1], S), writeln(S). 
