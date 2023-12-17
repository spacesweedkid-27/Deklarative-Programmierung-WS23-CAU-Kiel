%working_directory(CWD,'/Users/nikepulow/personal/Latex/gitclones/Deklarative-Programmierung-WS23-CAU-Kiel/H8').
% H8.2

person(skywalker).
person(solo).
person(mothma).
person(organa).

role(chairman).
role(traesurer).
role(secretary).

% solo und mothma wollen nicht gemeinsam in den Vorstand
cond1(C,T,S) :- (C \= solo, T \= solo, S \= solo) ; (C \= mothma, T \= mothma, S \= mothma). % passt

% solo nur im Vorstand, wenn Organa Vorsitzende
cond2(C,T,S) :- (T = solo, C = organa) ; (S = solo , C = organa) ; (T \= solo, S \= solo, C \= solo). % passt

% Organa nur im Vorstand, wenn Skywalker im Vorstand
cond3(C,T,S) :- ((C = organa, T = organa, S = organa) , (C = skywalker, T = skywalker, S = skywalker)) ; (C \= organa, T \= organa, S \= organa). % nicht notwendig

% Skywalker nicht im Vorstand, wenn Solo Kasse
cond4(C,T,S) :- ((T = solo), (C \= skywalker, T \= skywalker, S \= skywalker)) ; (T \= solo). % passt

% Mothma nicht im Vorstand, wenn Organa Sekretär
cond5(C,T,S) :- ((S = organa), (C \= mothma, T \= mothma, S \= mothma)) ; (S \= organa). % passt

board(C,T,S) :- person(C), person(T), person(S), person(skywalker), person(solo), person(mothma), person(organa),
                C \= T, C \= S, T \= S, (cond2(C,T,S)), (cond1(C,T,S)), (cond4(C,T,S)), (cond5(C,T,S)).