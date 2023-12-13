%working_directory(CWD,'/Users/nikepulow/personal/Latex/gitclones/Deklarative-Programmierung-WS23-CAU-Kiel/H8').

% Definition der Verheiratet-Relation:
verheiratet(christine, heinz).
verheiratet(hubert,    fritz).
verheiratet(monika,    angelika).
verheiratet(herbert,   maria).
verheiratet(claudia,   kim).
verheiratet(karolin, andreastante).

geschlecht(andreastante, w).
geschlecht(christine, w).
geschlecht(maria, w).
geschlecht(monika, w).
geschlecht(claudia, w).
geschlecht(anna, w).
geschlecht(susanne, w).
geschlecht(karolin, w).
geschlecht(angelika, d).
geschlecht(heinz, m).
geschlecht(fritz, m).
geschlecht(hubert, m).
geschlecht(herbert, m).
geschlecht(kim, m).
geschlecht(andreas, m).
geschlecht(norbert, m).

% Definition der Kind-Eltern-Relation:
kind(herbert,  christine, heinz).
kind(angelika, christine, heinz).
kind(maria,    hubert,    fritz).
kind(karolin,  hubert,    fritz).
kind(susanne,  monika,    angelika).
kind(kim,      monika,    angelika).
kind(andreas,  herbert,   maria).
kind(anna,     claudia,   kim).

vater(Kind,Vater) :- kind(Kind,Vater,_), geschlecht(Vater,m).
vater(Kind,Vater) :- kind(Kind,_,Vater), geschlecht(Vater,m).

mutter(Kind,Mutter) :- kind(Kind,Mutter,_), geschlecht(Mutter,w).
mutter(Kind,Mutter) :- kind(Kind,_,Mutter), geschlecht(Mutter,w).

% der Vollständigkeit halber, falls das irgendwo benötigt wird
elternteil(Kind,Elternteil) :- kind(Kind,Elternteil,_), geschlecht(Elternteil,d).
elternteil(Kind,Elternteil) :- kind(Kind,_,Elternteil), geschlecht(Elternteil,d).

eltern(Kind,Eltern) :- kind(Kind,Eltern,_).
eltern(Kind,Eltern) :- kind(Kind,_,Eltern).

% A1.1
grossmutter(Person,Grossmutter) :- kind(Person,Mutter,_), mutter(Mutter,Grossmutter).
grossmutter(Person,Grossmutter) :- kind(Person,_,Vater), mutter(Vater,Grossmutter).

% A1.2
geschwister(Person,Geschwister) :- kind(Person,Mutter,Vater), kind(Geschwister,Mutter,Vater), \+ Geschwister = Person.

% A1.3
bruder(Person,Bruder) :- geschwister(Person,Bruder), geschlecht(Bruder,m), \+ Person = Bruder.
% li'l helper for A1.4
schwester(Person,Schwester) :- geschwister(Person,Schwester), geschlecht(Schwester,w), \+ Person = Schwester.

% A1.4
% I have no knowledge about family-trees so I guess if a Person A is married to another person B
% and they have a parent who is married to a woman, that woman still counts as a aunt to Person A?
% if not, ignore the last two lines
tante(Person,Tante) :- eltern(Person,Eltern), schwester(Eltern,Tante).
tante(Person,Tante) :- eltern(Person,Eltern), geschwister(Eltern,ElGeschw), verheiratet(ElGeschw,Tante), geschlecht(Tante,w).
tante(Person,Tante) :- verheiratet(Person,Partner), eltern(Partner,Schwiegereltern), schwester(Schwiegereltern,Tante).
tante(Person,Tante) :- verheiratet(Person,Partner), eltern(Partner,Schwiegereltern), geschwister(Schwiegereltern,SchwElGeschw), verheiratet(SchwElGeschw,Tante), geschlecht(Tante,w).