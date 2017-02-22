op(700, xfx, ..).
op(1050, xfy, ->).
op(800, fy, if).
op(801, xfy, then).
op(802, xfy, else).
op(500, xfx, >).
op(500, xfx, <).

var(X) :- builtIn(var, X).
nonvar(X) :- builtIn(nonvar, X).
atomic(X) :- builtIn(atomic, X).
bound(X) :- builtIn(bound, X).
copy_term(X, Y) :- builtIn(copy_term, X, Y).
copy_term2(X, Y) :- assertz(cp_term(X)), retract(cp_term(Y)).

X =.. Y :- builtIn(univ, X, Y).
X is Y :- builtIn(is, X, Y).
X > Y :- builtIn( > , X, Y).
X < Y :- builtIn( < , X, Y).

assertz(X) :- builtIn(assertz, X).
retract(X) :- builtIn(retract, X).

X ; _ :- X.
_ ; Y :- Y.

and(X, Y) :- X, Y.

X -> Y :- X, !, Y.

p_case(X) :- X is 1 -> writeln('one') ; X is 2 -> writeln('two').

gg :- assertz(like(peter, mary)), assertz(like(peter, susan)), fail.
gg :- like(peter, Who), writeln(Who), fail.

if X then Y else Z :- X -> Y ; Z.

m2(X, X .. X) :- !.
m2(X, X .. Y).
m2(Z, X .. Y) :- X2 is X + 1, m2(Z, X2 .. Y).

m([X], X .. X).
m([X|Z], X .. Y) :- X2 is X + 1, m(Z, X2 .. Y).

not(G) :- G, !, fail.
not(G).
eq(X, X).

member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

g(Z) :- member(X, [1,2,3]), Y is X * 2, Z is Y * 3.

each(X, Goal, Act) :- Goal, Act, fail.

append([], L, L).
append([H|T], L, [H|L2]) :- append(T, L, L2).

