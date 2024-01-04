% 9.2.1
% ds(   f(g(X,Y),Z,h(Z)),
%       f(Z,g(Y,X),h(g(a,b)))) = {Z,g(X,Y)}
% s1 = {Z -> g(X,Y)}
% ds(   f(g(X,Y),g(X,Y),h(g(X,Y))),
%       f(g(X,Y),g(Y,X),h(g(a,b)))) = {Y,X}
% s2 = {Y -> X}
% ds(   f(g(X,X),g(X,X),h(g(X,X))),
%       f(g(X,X),g(X,X),h(g(a,b)))) = {X,a}
% s3 = {X -> a}
% ds(   f(g(a,a),g(a,a),h(g(a,a))),
%       f(g(a,a),g(a,a),h(g(a,b)))) -> Nicht unifizierbar, da a und b Konstanten

% 9.2.2
% ds(   f(X,g(X)),
%       f(g(Y),Y)) = {X,g(Y)}
% s1 = {X -> g(Y)}
% ds(   f(g(Y),g(g(Y))),
%       f(g(Y),Y))) = {Y,g(g(Y))} -> Nicht unifizierbar, da Variable in Term

% 9.2.3
% ds(   f(B,C,D),
%       f(g(A,A),g(B,B),g(C,C))) = {B,g(A,A)}
% s1 = {B -> g(A,A)}
% ds(   f(g(A,A),C,D),
%       f(g(A,A),g(g(A,A),g(A,A)),g(C,C))) = {C,g(g(A,A),g(A,A))}
% s2 = {C -> g(g(A,A),g(A,A)}
% ds(   f(g(A,A),g(g(A,A),g(A,A)),D),
%       f(g(A,A),g(g(A,A),g(A,A)),g(g(g(A,A),g(A,A),g(g(A,A),g(A,A))))) = {D,g(g(g(A,A),g(A,A),g(g(A,A),g(A,A)))}
% s3 = {D -> g(g(g(A,A),g(A,A),g(g(A,A),g(A,A)))}
% ds(   f(g(A,A),g(g(A,A),g(A,A)),g(g(g(A,A),g(A,A),g(g(A,A),g(A,A)))),
%       f(g(A,A),g(g(A,A),g(A,A)),g(g(g(A,A),g(A,A),g(g(A,A),g(A,A))))) = _ -> Unifizierbar mit s1.s2.s3 wie angegeben.