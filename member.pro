member(X, [X|_]).
member(X, [_|L]) :-  member(X, L).     

?-  member(b,[a,b,c]).

?-  member(X,[a,b,c]), writeln(X), allresult.