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
% Ich bin mir nicht sicher, ob ich die Aufgabenstellung hier richtig verstanden
% habe - meine Interpretation ist die Forderung, dass beide Listen R und L gleich sein
% müssen und keine Duplikate enthalten dürfen. Also anders als bei Remove werden auch
% keine Werte gestrichen, sondern direkt zu falsch ausgewertet, sobald in einer der Listen
% ein Duplikat gefunden wurde.
nub([],[]).                                             % ist wahr, wenn beide Listen leer sind
nub([Y|Xs],[Y|Ys]) :- \+ member(Y,Xs), nub(Xs,Ys).      % das erste Element beider Listen muss gleich sein und
                                                        % es wird geprüft, ob die Listen Duplikate enthalten,
                                                        % nach dem gleichen Schema wie bereits bei nodup.

% nub([1,2],[1,2]) >>> true
% nub([1,1],[1,1]) >>> false