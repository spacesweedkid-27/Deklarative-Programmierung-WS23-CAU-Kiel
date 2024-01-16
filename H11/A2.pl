% code from the script to line 7

add(o, Y, Y).                       % 0 is the neutral element of addition
add(s(X), Y, s(Z)) :- add(X,Y,Z).   % (a+1)+b = (c+1) <==> a+b = c

mult(o, _, o).                                  % 0 is the "killing" element of multiplication
mult(s(N), M, K) :- mult(N,M,O), add(O,M,K).    % add all sub-multiplications together.

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
