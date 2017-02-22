drop(X,[X|Xs],1,Xs).
drop(X,[Y|Xs],K,[Y|Ys]) :- K > 1, 
   K1 is K - 1, drop(X,Xs,K1,Ys).

?-  
N is 3,
drop(X,[a,b,c,d,e],N,R), 
writeln(X),
writeln(R).

