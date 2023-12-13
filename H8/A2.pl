%working_directory(CWD,'/Users/nikepulow/personal/Latex/gitclones/Deklarative-Programmierung-WS23-CAU-Kiel/H8').
% H8.2

person(skywalker).
person(solo).
person(organa).
person(mothma).

% dislikes for positions
secre(organa,mothma).
treas(solo,skywalker).

% general likes
genboard(skywalker,organa).
genboard(solo,mothma).
genboard(mothma,solo).

% likes for positions
chair(organa,solo).

board(Secretary,Traesurer,Chairman) :- secre()