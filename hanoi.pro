hanoi(N) :- move(N, left, middle, right).
move(1, A, U, C) :- inform(A, C), !.
move(N, A, B, C) :- 
	N1 is N - 1, 
	move(N1, A, C, B),
	inform(A, C), 
	move(N1, B, A, C).
 
inform(P1, P2) :- write(P1), write(' -> '), writeln(P2).

?- hanoi(5).