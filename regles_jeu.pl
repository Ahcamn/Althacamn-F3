% TO DO :
% - row
% - column
% - diagonal

:- module(mod_regles_jeu, [win/2, numP/3, move/3, setP/3, get_opponent/2]).

%Conditions de victoires
%3 pions sur une ligne
win(Plr, [Plr,Plr,Plr,_,_,_,_,_,_]) :- Plr \= 0, !.
win(Plr, [_,_,_,Plr,Plr,Plr,_,_,_]) :- Plr \= 0, !.
win(Plr, [_,_,_,_,_,_,Plr,Plr,Plr]) :- Plr \= 0, !.

%3 pions sur une colonne
win(Plr, [Plr,_,_,Plr,_,_,Plr,_,_]) :- Plr \= 0, !.
win(Plr, [_,Plr,_,_,Plr,_,_,Plr,_]) :- Plr \= 0, !.
win(Plr, [_,_,Plr,_,_,Plr,_,_,Plr]) :- Plr \= 0, !.

%3 pions sur une diagonale
win(Plr, [Plr,_,_,_,Plr,_,_,_,Plr]) :- Plr \= 0, !.
win(Plr, [_,_,Plr,_,Plr,_,Plr,_,_]) :- Plr \= 0, !.


%position de la case déplaçable en fonction de la case vide
empty(0, 1).
empty(0, 3).
empty(1, 0).
empty(1, 2).
empty(1, 4).
empty(2, 1).
empty(2, 5).
empty(3, 0).
empty(3, 4).
empty(3, 6).
empty(4, 1).
empty(4, 3).
empty(4, 5).
empty(4, 7).
empty(5, 2).
empty(5, 4).
empty(5, 8).
empty(6, 3).
empty(6, 7).
empty(7, 4).
empty(7, 6).
empty(7, 8).
empty(8, 5).
empty(8, 7).

%retourne l'élément central de deux cases opposées pour le déplacement de 2 cases
empty2(0, 6, 3).
empty2(6, 0, 3).
empty2(1, 7, 4).
empty2(7, 1, 4).
empty2(3, 5, 4).
empty2(5, 3, 4).
empty2(2, 8, 5).
empty2(8, 2, 5).
empty2(0, 2, 1).
empty2(2, 0, 1).
empty2(6, 8, 7).
empty2(8, 6, 7).

%Modification de l'élément voulu du plateau
modifyBoard(Val, 0, [_|R], [Val|R]) :- !.
modifyBoard(Val, I, [X|R1], [X|R2]) :-
  I > 0, I is I-1, modifyBoard(Val, I, R1, R2), !.


%nombre de pions placés pas le joueur Plr
numP(Plr, B, N) :- sublist(=(Plr), B, L), length(L, N).


% Poser un pion (max 3)
move(Plr, B, [-1, T, 0], NewB) :-
  %garde-fou ici ou dans setP (ou pas du tout)?
  setP(Plr, B, T),
  modifyBoard(Plr, T, B, NewB).

%pour pose de pion
setP(Plr, [0|R], 0) :- numP(Plr, [0|R], N), N<3.
setP(Plr, [T0, 0|R], 0) :- numP(Plr, [T0, 0|R], N), N<3.
setP(Plr, [T0, T1, 0|R], 0) :- numP(Plr, [T0, T1, 0|R], N), N<3.
setP(Plr, [T0, T1, T2, 0|R], 0) :- numP(Plr, [T0, T1, T2, 0|R], N), N<3.
setP(Plr, [T0, T1, T2, T3, 0|R], 0) :- numP(Plr, [T0, T1, T2, T3, 0|R], N), N<3.
setP(Plr, [T0, T1, T2, T3, T4, 0|R], 0) :- numP(Plr, [T0, T1, T2, T3, T4, 0|R], N), N<3.
setP(Plr, [T0, T1, T2, T3, T4, T5, 0|R], 0) :- numP(Plr, [T0, T1, T2, T3, T4, T5, 0|R], N), N<3.
setP(Plr, [T0, T1, T2, T3, T4, T5, T6, 0|R], 0) :- numP(Plr, [T0, T1, T2, T3, T4, T5, T6, 0|R], N), N<3.
setP(Plr, [T0, T1, T2, T3, T4, T5, T6, T7, 0|R], 0) :- numP(Plr, [T0, T1, T2, T3, T4, T5, T6, T7, 0|R], N), N<3.


% Déplacer un pion (TE = case d'arrivé, TS = case de départ)
move(Plr, B, [TS, TE, 1], NewB) :-
  %garde-fou ici ou dans moveP (ou pas du tout)?
  moveP(Plr, B, [TS, TE]),
  modifyBoard(0, TS, B, Temp),
  modifyBoard(Plr, TE, Temp, NewB).
  
%pour déplacement de pion  
moveP(Plr, B, [TS, TE]) :-
  !, ,nth0(TS, B, Plr), nth0(TE, B, 0).

% Déplacer une case
move(_, B, [TS, TE, 2], NewB) :-
  %garde-fou ici ou dans moveT (ou pas du tout)?
  moveT(B, [TS, TE]),
  modifyBoard(-1, TE, B, Temp),
  nth0(TE, B, Val),
  modifyBoard(Val, TS, Temp, NewB).

%pour déplacement de case
moveT(B, [TS, TE]) :-
  !, empty(TS, TE), nth0(TS, B, -1).

% Déplacer deux cases
move(_, B, [TS, TE, 3], NewB) :-
  %garde-fou?
  empty2(TS, TE, TI),
  move(_, B, [TS, TI, 2], Temp1),
  move(_, Temp1, [TI, TE, 2], NewB).

get_opponent(1, 2) :- !.
get_opponent(2, 1).
