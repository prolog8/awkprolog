length([], 0).
length([_|L], N) :- length(L, N1), N is N1 + 1.

?- length([a, b, c, d, e], LEN), writeln(LEN).
