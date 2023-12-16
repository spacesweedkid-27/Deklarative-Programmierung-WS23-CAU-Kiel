% !! DOUBLE NEGATION DOES NOT YET WORK !! %


%  and
and(false, _) :- false.
and(_, false) :- false.
and(true, true) :- true.

% or
or(true, _) :- true.
or(_, true) :- true.
or(false, false) :- false.

% not
not(false) :- true.
not(true) :- false.

ex1(X, Y, Z, Res) :- Res = or(and(X, Y), Z).