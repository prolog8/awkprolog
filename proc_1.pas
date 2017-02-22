% seq action in block
?- L = [begin, id("X"), eq, const(5), add, const(3), sep, id("Y"), eq, id("X"), sub, const(2), end], block(T, L, R), writeln(T).

% if-then-else
?- L = [if, id("X"), eq, const(3), add, id("Y"), then, begin, id("X"), eq, const(3), end, else, begin, id("X"), eq, const(4), end], actions(T, L, R), writeln(T).
