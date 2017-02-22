eat(err, L, L) :- !.
eat(X, [X|T], T) :- !.
true.

%*******************************************************
block(Va, Vb, Vc) :- 
    eat(begin, Vb, Vd), actions(Va, Vd, Ve), eat(end, Ve, Vc).
actions([Va|Vb], Vc, Vd) :- 
    action(Va, Vc, Ve), more_actions(Vb, Ve, Vd).
more_actions(Va, Vb, Vc) :- 
    eat(sep, Vb, Vd), actions(Va, Vd, Vc).
more_actions([], Va, Va) :- 
    true.
action(if(Va, Vb, Vc), Vd, Ve) :- 
    eat(if, Vd, Vf), cond(Va, Vf, Vg), eat(then, Vg, Vh), block(Vb, Vh, Vi), eat(else, Vi, Vj), block(Vc, Vj, Ve).
action(while(Va, Vb), Vc, Vd) :- 
    eat(while, Vc, Ve), cond(Va, Ve, Vf), eat(do, Vf, Vg), block(Vb, Vg, Vd).
action(assign(Va, Vb), Vc, Vd) :- 
    eat(id(Va), Vc, Ve), eat(eq, Ve, Vf), expr(Vb, Vf, Vd).
expr(Va, Vb, Vc) :- 
    term(Vd, Vb, Ve), exprtail(Vd, Va, Ve, Vc).
exprtail(Va, add(Va, Vb), Vc, Vd) :- 
    eat(add, Vc, Ve), term(Vf, Ve, Vg), exprtail(Vf, Vb, Vg, Vd).
exprtail(Va, sub(Va, Vb), Vc, Vd) :- 
    eat(sub, Vc, Ve), term(Vf, Ve, Vg), exprtail(Vf, Vb, Vg, Vd).
exprtail(Va, mul(Va, Vb), Vc, Vd) :- 
    eat(mul, Vc, Ve), term(Vf, Ve, Vg), exprtail(Vf, Vb, Vg, Vd).
exprtail(Va, div(Va, Vb), Vc, Vd) :- 
    eat(div, Vc, Ve), term(Vf, Ve, Vg), exprtail(Vf, Vb, Vg, Vd).
exprtail(Va, Va, Vb, Vb) :- 
    true.
term(id(Va), Vb, Vc) :- eat(id(Va), Vb, Vc).
term(const(Va), Vb, Vc) :- eat(const(Va), Vb, Vc).

cond(Va, Vb, Vc) :- eat(id(Vd), Vb, Ve), ctail(Vd, Va, Ve, Vc).
ctail(Va, eq(Va, Vb), Vc, Vd) :- eat(eq, Vc, Ve), expr(Vb, Ve, Vd).
ctail(Va, ne(Va, Vb), Vc, Vd) :- eat(ne, Vc, Ve), expr(Vb, Ve, Vd).
ctail(Va, idtest(Va), Vb, Vb) :- true.
