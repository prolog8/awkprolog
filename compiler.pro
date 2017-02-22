cg(I, [pushc(I)]) :- number(I).
cg(A, [push(A)]) :- atomic(A).

cg(add(X, Y), [CX, CY, add]) :- cg(X, CX), cg(Y, CY).
cg(sub(X, Y), [CX, CY, sub]) :- cg(X, CX), cg(Y, CY).
cg(mul(X, Y), [CX, CY, mul]) :- cg(X, CX), cg(Y, CY).
 
cg(cmp(s_gt, X, Y), [CX, CY, gt]) :- cg(X, CX), cg(Y, CY).
cg(cmp(s_lt, X, Y), [CX, CY, lt]) :- cg(X, CX), cg(Y, CY).
cg(cmp(s_equ, X, Y), [CX, CY, equ]) :- cg(X, CX), cg(Y, CY).
cg(cmp(not_equ, X, Y), [CX, CY, not_equ]) :- cg(X, CX), cg(Y, CY).
cg(cmp(s_ge, X, Y), [CX, CY, ge]) :- cg(X, CX), cg(Y, CY).
cg(cmp(s_le, X, Y), [CX, CY, le]) :- cg(X, CX), cg(Y, CY).

cg(func(main, X, Y), [CY, halt]) :- cg(Y, CY).
cg(func(F, X, Y), [pushc(R1), pop(F), bz(R2), label(R1), CY, ret, label(R2)]) :- cg(Y, CY).

cg(assign(X, Y), [CY, pop(X)]) :- cg(Y, CY).
cg(while_stm(X, S), [label(R1), CX, bz(R2), SX, br(R1), label(R2)]) :- 
	cg(X, CX), cg(S, SX).
	
cg([], []).
cg([A|B], [CA, CB]) :- cg(A, CA), cg(B, CB).


append([], L, L).
append([H|T], L, [H|L2]) :- append(T, L, L2).

flatten([], []).
flatten([H|T], L3) :- flatten(H, L1), flatten(T, L2), append(L1, L2, L3).
flatten(X, [X]).

find_symbol([], D, D) :- !.
find_symbol([push(X) | T], D, D3) :- add_sym(X, D, D2), find_symbol(T, D2, D3).
find_symbol([pop(X) | T], D, D3) :- add_sym(X, D, D2), find_symbol(T, D2, D3).
find_symbol([H | T], D, DD) :- find_symbol(T, D, DD).

member(X, [X | L]) :- !.
member(X, [Y | L]) :- member(X, L).

add_sym(X, L, L) :- member(X, L).
add_sym(X, L, [X | L]).

allocate([], N, []).
allocate([H | T], N, [sym(H, N) | T2]) :- N2 is N + 1, allocate(T, N2, T2).

relocate([], SYM, []) :- !.
relocate([push(X) | T], D, [push(Y) | T2]) :- 
	member(sym(X, Y), D), relocate(T, D, T2).
relocate([pop(X) | T], D, [pop(Y) | T2]) :- 
	member(sym(X, Y), D), relocate(T, D, T2).
relocate([H | T], D, [H | T2]) :- relocate(T, D, T2).

out_code([], N) :- !.
out_code([label(A)|T], N) :- !, out_code(T, N).
out_code([H|T], N) :- !, write(N), write(':'), writeln(H), N2 is N + 1, out_code(T, N2).

ass([], N, N, D, D) :- !.
ass([label(A)|T], A, B, D, D2) :- !, ass(T, A, B, D, D2).
ass([H|T], N, NN, D, D2) :- !, N2 is N + 1, ass(T, N2, NN, D, D2).


gen(T) :- 
	writeln('compile...'), cg(T, C), flatten(C, L),
	writeln('assemble...'), ass(L, 0, HEAP, [], FUN_LIST),
	writeln('generate symbol table...'), find_symbol(L, [], SYM_LIST),
	writeln('allocate...'), allocate(SYM_LIST, HEAP, SYM_ADDR),
	writeln('relocate...'), relocate(L, SYM_ADDR, L2),
	out_code(L2, 0),
	writeln(SYM_ADDR),
	writeln(FUN_LIST).






