notequal(X, X) :- !, fail.
notequal(X, Y).

length([], 0).
length([_|L], N) :- length(L, N1), N is N1 + 1.

pack([],[]).
pack([X|Xs],[Z|Zs]) :- transfer(X,Xs,Ys,Z), pack(Ys,Zs).

transfer(X,[],[],[X]).
transfer(X,[Y|Ys],[Y|Ys],[X]) :- notequal(X, Y).
transfer(X,[X|Xs],Ys,[X|Zs]) :- transfer(X,Xs,Ys,Zs).

transform([],[]).
transform([[X|Xs]|Ys],[[N,X]|Zs]) :- length([X|Xs],N), transform(Ys,Zs).

encode(L1,L2) :- pack(L1,L), transform(L,L2).



?-  encode([a,a,b,c,c,c,a,a,d,e,e,e,e],X), writeln(X).