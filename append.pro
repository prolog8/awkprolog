append([], L, L).
append([H|T], L, [H|L2]) :- append(T, L, L2).

?- append([a, b], [c, d], C), writeln(C).
?- append(A, B, [a, b, c, d]), write(A), write(', '), writeln(B), fail.
