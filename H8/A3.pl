% 8.3)

% 8.3.1)
%  and
and(false, _, false).
and(_, false, false).
and(true, true, true).

% or
or(true, _, true).
or(_, true, true).
or(false, false, false).

% not
not(true, false).
not(false, true).

% take and(X,Y) and pass to or(Z, ).
ex1(X, Y, Z, Res) :-
    and(X, Y, Res1),
    or(Res1, Z, Res).


% resolve left-associative binary parser tree.
ex2(X, Y, Z, Res) :-
    and(X, Y, Res1),
    and(Y, Z, Res2),
    or(Res2, Z, Res3),
    or(Res1, Res3, Res).


% coding like this seems very confusing...
ex3(X, Y, Z, Res) :-
    not(Y, Res1),
    and(X, Res1, Res2),
    and(Res2, Z, Res3),
    and(Z, Y, Res4),
    or(Res4, Z, Res5),
    or(Res3, Res5, Res).


% 8.3.2)
% Pass the queries:
%   ex1(true, false, true, Res).
%   ex2(true, false, true, Res).
%   ex3(true, false, true, Res).
%
% to get the responses:
%   Res = true
%   Res = true
%   Res = true

% 8.3.3)
% To get true you have to pass X,Y,Z that hold the following criteria:
% ex1: X,Y true or Z true.
% this can be found out with the following queries:
%   ex1(X, Y, Z, true).
% ->  
%   X = false,
%   Z = true
%
%   ex1(true, Y, Z, true).
% ->
%   Y = false,
%   Z = true
%
%   ex1(X, Y, false, true).
% ->
%   X = Y, Y = true
%
% ex2: X,Y true or Z true
%   ex2(X, Y, Z, true).
% ->
%   X = Y, Y = false,
%   Z = true
%
%   ex2(X, Y, false, true).
% ->
%   X = Y, Y = true
%
% ex3: Z should be true
%   ex3(false, false, Z, true).
% ->
%   Z = true
%
%   ex3(X, Y, true, true).
% ->
%   X = false,
%   Y = true
%
%   ex3(true, false, true, true).
% ->
%   true