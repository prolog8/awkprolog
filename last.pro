last(X, [X]).
last(X, [_|L]) :- last(X, L).

?- last(X, [a, b, c, d, e]), writeln(X).
