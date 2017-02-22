qsort([], []).
qsort([X|T], S) :-
	split(X, T, SM, BI),
	qsort(SM, SSM),
	qsort(BI, SBI),
	conc(SSM, [X|SBI], S).
	
split(X, [], [], []).
split(X, [Y|T], [Y|S], B) :- X > Y, !, split(X, T, S, B).
split(X, [Y|T], S, [Y|B]) :- split(X, T, S, B).

conc([],L,L).
conc([X|L1],L2,[X|L3]) :- conc(L1,L2,L3).
 
?- qsort([6, 45, 30, 21, 23, 20, 8, 7, 32, 1], S), writeln(S). 