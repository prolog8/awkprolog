color(red).
color(green).
color(blue).
color(yellow).
 
n(StateAColor, StateBColor) :- color(StateAColor), 
    color(StateBColor), 
    not(eq(StateAColor, StateBColor)). 

not(G) :- G, !, fail.
not(G).
eq(X, X).

%*********************************
% Define states in a country
%*********************************

country(S1, S2, S3, S4, S5) :-

n(S1, S2), n(S1, S3), n(S1, S4), n(S1, S5),
n(S2, S1), n(S2, S3), n(S2, S4),
n(S3, S1), n(S3, S2), n(S3, S4),
n(S4, S1), n(S4, S2), n(S4, S3), n(S4, S5),
n(S5, S1), n(S5, S4).


?-	R is country(S1, S2, S3, S4, S5), 
	R,
	writeln(R).
