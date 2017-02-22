fac(1, 1) :- !.
fac(N, F) :- N2 is N - 1, fac(N2, F2), F is F2 * N.


?- fac(5, R), writeln(R).

