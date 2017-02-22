human(david).
human(john).
human(suzie).
human(eliza).
man(david).
man(john).
woman(suzie).
woman(eliza).
parent(david, john).
parent(john, eliza).
parent(suzie, eliza).

father(X,Y) :- parent(X,Y), man(X).
mother(X,Y) :- parent(X,Y), woman(X).

?- father(Who, eliza), writeln(Who).

?- mother(Mother,Child), writeln(Mother), writeln(Child).