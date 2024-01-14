% 10.2 Negation als Fehlschlag für Listen

% nodup(L) ist erfüllbar, wenn die Liste L frei von Duplikaten ist
nodup([]).                                      % erfüllbar, wenn Liste leer ist
nodup([X | Xs]) :- \+ member(X,Xs), nodup(Xs).

% neq(X,Y) ist erfüllbar, wenn X und Y nicht unifizierbar sind
neq(X,Y) :- \+ X = Y.

% remove(X,Xs,Ys) ist erfüllbar, wenn das Streichen aller Vorkommen von X aus der Liste Xs der Liste Ys entspricht
remove(_,[],[]).                                        % ist wahr, wenn Listen und Element leer sind
remove(X, [X|Xs], Ys)       :- remove(X,Xs,Ys).         % ist das erste Element der Liste Xs == X,
                                                        % mach mit dem Rest der Liste weiter, X wird aus Xs entfernt
remove(X,[Y|Xs],[Y|Ys])     :- X \= Y, remove(X,Xs,Ys). % Ist X nicht das in beiden Listen aktuell betrachtete erste
                                                        % Element, kann mit den Restlisten fortgefahren werden, sofern
                                                        % diese gleich sind

% nub(L,R) ist erfüllbar, wenn R der Liste L ohne Duplikate entspricht
nub([],[]).                 % ist wahr, wenn beide Listen leer sind
nub([X|Xs],Ys) :- remove(X,Xs,Yss), nub(Yss,Ys). % no, that's not it