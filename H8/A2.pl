%working_directory(CWD,'/Users/nikepulow/personal/Latex/gitclones/Deklarative-Programmierung-WS23-CAU-Kiel/H8').
% H8.2

person(skywalker).
person(solo).
person(organa).
person(mothma).

skywalker(Role).
solo(Role).
organa(Role).
mothma(Role).

% roles
roles(secretary).
roles(traesurer).
roles(chairman).
roles(boardmember).

%dislikes(solo,boardmember(mothma)).
%dislikes(skywalker,traesurer(solo)).
%dislikes(mothma,secretary(organa)).
%likes(organa,boardmember(skywalker)).
%likes(solo,chairman(organa)).

%skywalker(roles(secretary)) :- na
%skywalker(roles(traesurer)) :- na
%skywalker(roles(chairman)) :- na
organa(roles(_)) :- skywalker(roles(_)).
organa(roles(secretary)) :- \+ mothma(roles(_)).
organa(roles(chairman)) :- solo(roles(_)).
%organa(roles(traesurer)) :- na
solo(roles(_)) :- \+ mothma(roles(_)), organa(roles(chairman)).
solo(roles(traesurer)) :- skywalker(roles(_)).
mothma(roles(_)) :- \+ solo(roles(_)).
mothma(roles(_)) :- \+ organa(roles(secretary)).

board(Person,Role) :- 