rev(L1, L2) :- rev2(L1, L2, []).

rev2([],L,L).
rev2([X|Xs], L2, Acc) :- rev2(Xs, L2, [X|Acc]).

?- rev([a, b, c, d, e], L), writeln(L).
