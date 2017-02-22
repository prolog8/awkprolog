father(johnny, thomas).
father(samuel, david).
father(james, kevin).
father(thomas, edward).
father(david, leo).
father(david, peter).
father(david, gordan).
father(edward, joe).

grandfather(X, Z) :- father(X, Y), father(Y, Z).

?- grandfather(Who, peter), writeln(Who).

?- grandfather(samuel, Who), writeln(Who).

?- grandfather(samuel, Who), writeln(Who), printmore.