% isPeano(o).
% isPeano(s(N)) :- isPeano(N).

% succ(X, s(X)).
% pred(s(X), X).

add(o, Y, Y).
add(s(X), Y, s(Z)) :- add(X,Y,Z).

% sub(X, Y, Z) :- add(Y, Z, X). % unused

mult(o, _, o).
mult(s(N), M, K) :- mult(N,M,O), add(O,M,K).

/* unused
leq(o, _).
leq(s(N), s(M)) :- leq(N,M).

geq(_, o).
geq(s(N), s(M)) :- leq(N,M).

eq(o, o).
eq(s(N), s(M)) :- eq(N,M).
*/

horseman(K, ResMult, K, o) :- mult(s(s(s(s(o)))), K, ResMult).   % if there are K heads and 4*K legs, we have K horses, inductive start
horseman(K, ResMult, o, K) :- mult(s(s(o)), K, ResMult).         % if there are K heads and 2*K legs, we have K humans, inductive start
horseman(s(K), s(s(s(s(B)))), s(Res1), Res2) :- horseman(K, B, Res1, Res2), !.  % if there are K+1 heads and B+4 legs, then calculate h(K,B) and add (1,0),
                                                                                % stop after this because there is only one solution,
                                                                                % but for a call of horseman there are always up to 2 subcalls,
                                                                                % this one and the next one.
                                                                                % inductive step
horseman(s(K), s(s(B)), Res1, s(Res2)) :- horseman(K, B, Res1, Res2), !.        % if there are K+1 heads and B+2 legs, then calculate h(K,B) and add (0,1)
                                                                                % (same reason for the cut)
                                                                                % inductive step
