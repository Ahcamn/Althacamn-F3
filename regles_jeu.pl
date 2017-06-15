:- module(mod_regles_jeu, [win/2, empty2/3, numP/3, moveP/3, moveT/3, move2T/3, setP/3, get_opponent/2, row/3, column/3, diagonal/3]).

% Conditions de victoires
% 3 pions sur une ligne
win(Plr, [Plr,Plr,Plr,_,_,_,_,_,_]) :- Plr \= 0, !.
win(Plr, [_,_,_,Plr,Plr,Plr,_,_,_]) :- Plr \= 0, !.
win(Plr, [_,_,_,_,_,_,Plr,Plr,Plr]) :- Plr \= 0, !.

% 3 pions sur une colonne
win(Plr, [Plr,_,_,Plr,_,_,Plr,_,_]) :- Plr \= 0, !.
win(Plr, [_,Plr,_,_,Plr,_,_,Plr,_]) :- Plr \= 0, !.
win(Plr, [_,_,Plr,_,_,Plr,_,_,Plr]) :- Plr \= 0, !.

% 3 pions sur une diagonale
win(Plr, [Plr,_,_,_,Plr,_,_,_,Plr]) :- Plr \= 0, !.
win(Plr, [_,_,Plr,_,Plr,_,Plr,_,_]) :- Plr \= 0, !.


% Position de la case déplaçable en fonction de la case vide
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

% Retourne l'élément central de deux cases opposées pour le déplacement de 2 cases
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

% Nombre de pions placés pas le joueur (Plr)
numP(Plr, B, N) :- 
    sublist(=(Plr), B, L), 
    length(L, N).
    

% Vérifie si le pion peut être posé
setP(Plr, [0|R], 0) :- numP(Plr, [0|R], N), N<3.
setP(Plr, [T0, 0|R], 1) :- numP(Plr, [T0, 0|R], N), N<3.
setP(Plr, [T0, T1, 0|R], 2) :- numP(Plr, [T0, T1, 0|R], N), N<3.
setP(Plr, [T0, T1, T2, 0|R], 3) :- numP(Plr, [T0, T1, T2, 0|R], N), N<3.
setP(Plr, [T0, T1, T2, T3, 0|R], 4) :- numP(Plr, [T0, T1, T2, T3, 0|R], N), N<3.
setP(Plr, [T0, T1, T2, T3, T4, 0|R], 5) :- numP(Plr, [T0, T1, T2, T3, T4, 0|R], N), N<3.
setP(Plr, [T0, T1, T2, T3, T4, T5, 0|R], 6) :- numP(Plr, [T0, T1, T2, T3, T4, T5, 0|R], N), N<3.
setP(Plr, [T0, T1, T2, T3, T4, T5, T6, 0|R], 7) :- numP(Plr, [T0, T1, T2, T3, T4, T5, T6, 0|R], N), N<3.
setP(Plr, [T0, T1, T2, T3, T4, T5, T6, T7, 0|R], 8) :- numP(Plr, [T0, T1, T2, T3, T4, T5, T6, T7, 0|R], N), N<3.

  
% Vérifie si le pion peut être bougé
moveP(Plr, B, [TS, TE]) :-
    empty(TS, TE),
    nth0(TS, B, Plr), 
    nth0(TE, B, 0).
    

% Vérifie si la case vide peut être déplacée
moveT(B, [PrevTS, PrevTE], [TS, TE]) :-
    %PrevTS \= TE,
    %PrevTE \= TS,
    empty(TS, TE),
    nth0(TS, B, -1).

move2T(B, [PrevTS, PrevTE], [TS, TE]) :-
    %PrevTS \= TE,
    %PrevTE \= TS,
    empty2(TS, TE),
    nth0(TS, B, -1).
    

% Retourne l'ID de l'adversaire
get_opponent(1, 2).
get_opponent(2, 1).


row(B, 1, [Y1, Y2, Y3]) :-
	nth0(0, B, Y1),
	nth0(1, B, Y2),
	nth0(2, B, Y3).
row(B, 2, [Y1, Y2, Y3]) :-
	nth0(3, B, Y1),
	nth0(4, B, Y2),
	nth0(5, B, Y3).
row(B, 3, [Y1, Y2, Y3]) :-
	nth0(6, B, Y1),
	nth0(7, B, Y2),
	nth0(8, B, Y3).

column(B, 1, [Y1, Y2, Y3]) :-
	nth0(0, B, Y1),
	nth0(3, B, Y2),
	nth0(6, B, Y3).
column(B, 2, [Y1, Y2, Y3]) :-
	nth0(1, B, Y1),
	nth0(4, B, Y2),
	nth0(7, B, Y3).
column(B, 3, [Y1, Y2, Y3]) :-
	nth0(2, B, Y1),
	nth0(5, B, Y2),
	nth0(8, B, Y3).

diagonal(B, 1, [Y1, Y2, Y3]) :-
	nth0(0, B, Y1),
	nth0(4, B, Y2),
	nth0(8, B, Y3).
diagonal(B, 2, [Y1, Y2, Y3]) :-
	nth0(2, B, Y1),
	nth0(4, B, Y2),
	nth0(6, B, Y3).
