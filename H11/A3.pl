mobile(fisch(A), A).
mobile(bruecke(A, B), Res) :- mobile(A, ResA),
                              mobile(B, ResB),
                              Res is ResA + ResB + 1.

%  ?- mobile(X,3).
% X = fisch(3);
% ERROR: Arguments are not sufficiently instantiated