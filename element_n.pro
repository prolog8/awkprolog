at(X, [X|_], 1).
at(X, [_|L], N) :- N > 1, N1 is N - 1, at(X,L,N1).

?- at(X, [a, b, c, d, e], 3), writeln(X).
