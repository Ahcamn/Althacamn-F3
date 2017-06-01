:- module(mod_regles_jeu, [win/2 ]).

%Conditions de victoires
%3 pions sur une ligne
win(Plr, [Plr,Plr,Plr,_,_,_,_,_,_]) :- Plr != 0, !.
win(Plr, [_,_,_,Plr,Plr,Plr,_,_,_]) :- Plr != 0, !.
win(Plr, [_,_,_,_,_,_,Plr,Plr,Plr]) :- Plr != 0, !.

%3 pions sur une colonne
win(Plr, [Plr,_,_,Plr,_,_,Plr,_,_]) :- Plr != 0, !.
win(Plr, [_,Plr,_,_,Plr,_,_,Plr,_]) :- Plr != 0, !.
win(Plr, [_,_,Plr,_,_,Plr,_,_,Plr]) :- Plr != 0, !.

%3 pions sur une diagonale
win(Plr, [Plr,_,_,_,Plr,_,_,_,Plr]) :- Plr != 0, !.
win(Plr, [_,_,Plr,_,Plr,_,Plr,_,_]) :- Plr != 0, !.


% Poser un pion


% Déplacer un pion


% Déplacer une case


% Déplacer deux cases


get_opponent(1, 2) :- !.
get_opponent(2, 1).
