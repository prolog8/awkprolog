member(X, [X|_]).
member(X, [_|L]) :-  member(X, L).

not(G) :- G, !, fail.
not(G).

path(X, X, T, [X]).
path(X, Z, T, [X|T2]) :- 
	p(X, Y), 
	not(member(Y, T)),
	path(Y, Z, [Y|T], T2).

min(A,B,A) :- A < B.
min(A,B,B).

% Pour water from A into B
pourAB(s(X,Y), s(X2, Y2)) :- 
	Y < 5,
	V1 is 5 - Y,
	V2 is X,
	min(V1,V2,V3),
	Y2 is Y + V3,
	X2 is X - V3.	


% Pour water from B into A
pourBA(s(X,Y), s(X2, Y2)) :- 
	X > 3,
	V1 is 3 - X,
	V2 is Y,
	min(V1,V2,V3),
	Y2 is Y - V3,
	X2 is X + V3.	

% Discard water from jug
clearA(s(X,Y), s(0,Y)) :- X > 0.
clearB(s(X,Y), s(X,0)) :- Y > 0.
% Completely fill jug
fullA(s(X,Y), s(3,Y)) :- X < 3.
fullB(s(X,Y), s(X,5)):- Y < 5.

p(A, B) :- clearA(A,B).
p(A, B) :- clearB(A,B).
p(A, B) :- fullA(A,B).
p(A, B) :- fullB(A,B).
p(A, B) :- pourAB(A,B).
p(A, B) :- pourBA(A,B).


?- path(s(0,0), s(0,4), [], P), writeln(P).

% ?- p(s(2, 0), B),p(B,C), p(C,D),writeln(D).


