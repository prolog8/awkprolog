%%%%%%%%%%%%%%%%%%%%%%%
% Eight-Queen Problem
%%%%%%%%%%%%%%%%%%%%%%%
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).
 
sol([]).
sol([pos(X, Y) | Other]) :-
	sol(Other),
	member(Y, [1,2,3,4,5,6,7,8]),
	noattack(pos(X, Y), Other).

noattack(_, []).
noattack(pos(X, Y), [pos(X1, Y1) | Other]) :-
	Y =\= Y1,
	Y1 - Y =\= X1 - X,
	Y1 - Y =\= X - X1,
	noattack(pos(X, Y), Other).

template([pos(1, Y1), pos(2, Y2), pos(3, Y), pos(4, Y4), pos(5, Y5), pos(6, Y6), pos(7, Y7), pos(8, Y8)]).

?- template(S), sol(S), writeln(S).