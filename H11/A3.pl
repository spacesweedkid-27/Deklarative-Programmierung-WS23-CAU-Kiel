mobile(fisch(A), A) :- A >= 0.
mobile(bruecke(A, B), Res) :- mobile(A, ResA),
                              mobile(B, ResB),
                              Res is ResA + ResB + 1.

% ?- mobile(bruecke(fisch(1),fisch(1)),2).
% -> false.
%
% ?- mobile(bruecke(fisch(1),fisch(1)),3).
% -> true.
%
% ?- mobile(X,3).
% X = fisch(3);
% ERROR: Arguments are not sufficiently instantiated
%
% Prolog fails this query because after finding X = fisch(3)
% it will backtrack to resolve
% ?- mobile(bruecke(A, B), 3)
% |-> mobile(A, ResA), mobile(B, ResB), 3 is ResA + ResB + 1.
% |-> mobile(bruecke(fisch(ResA),fisch(ResB)),3), 3 is ResA + ResB + 1.
% this will fail because ResA and ResB are unknown, the only thing that is known,
% is that they will have to satisfy 3 is ResA + ResB + 1, but because ResA and ResB are not instanciated,
% Prolog will fail, since it can't solve the equation "3 = ResA + ResB + 1" with is.
% If one would use peano-numbers, it would be possible to find solutions,
% since with them, all solutions for add(A, B, s(o(o))) (remember 3-1 = 2 ^= s(o(o))) would be found.
% you could also implement this with clpq, because it allows solving for uninstanciated variables.