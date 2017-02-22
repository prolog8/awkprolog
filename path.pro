a(a, c).
a(a, b).
a(b, f).
a(s, w).
a(y, a).
a(y, z).
a(w, u).
a(u, y).
a(c, w).
 
not(G) :- G, !, fail.
not(G).

path(X, X, T, [X]).
path(X, Z, T, [X|T2]) :- 
	a(X, Y), 
	not(member(Y, T)),
	path(Y, Z, [Y|T], T2).

member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

?-	path(s, z, [], P), 
	write('The path from s to z :'),
	writeln(P).
