pack([],[]).
pack([X|Xs],[Z|Zs]) :- transfer(X,Xs,Ys,Z), pack(Ys,Zs).

transfer(X,[],[],[X]).
transfer(X,[Y|Ys],[Y|Ys],[X]) :- notequal(X, Y).
transfer(X,[X|Xs],Ys,[X|Zs]) :- transfer(X,Xs,Ys,Zs).

notequal(X, X) :- !, fail.
notequal(X, Y).
 
?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X), writeln(X).