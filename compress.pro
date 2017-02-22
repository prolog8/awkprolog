compress([],[]).
compress([X],[X]).
compress([X,X|Xs],Zs) :- compress([X|Xs],Zs).
compress([X,Y|Ys],[X|Zs]) :- notequal(X, Y), compress([Y|Ys],Zs).

notequal(X, X) :- !, fail.
notequal(X, Y).

?- compress([a,a,a,a,b,c,c,a,a,d,d,e,e,e,e], X), writeln(X).