%TO DO:
%	removeOldMove

:- module(mod_ia, [evaluation_board/3, alpha_beta/7]).
:- use_module('regles_jeu.pl').
:- use_module('interface.pl').
:- use_module(library(lists)).


% evaluation_board(+B, +Plr, ?Val)
% Détermine qui est en train de gagner, les valeurs de 100 et -100
% représentent respectivement une victoire et une défaite. 
% Si une autre valeur est retournée, l'avantage sera déterminé en fonction 
% du signe de Val (si positif avantage pour le joueur actuel, sinon pour
% l'adversaire).
evaluation_board(B, Plr, 100) :-
	win(Plr, B),!.
evaluation_board(B, Plr, -100) :-
	get_opponent(Plr, Opp),
	win(Opp, B),!.
evaluation_board(B, Plr, Val) :- 
	get_opponent(Plr, Opp),
	evaluation_value(Plr, B, ValPlr),
	evaluation_value(Opp, B, ValOpp),
	Val is ValPlr - ValOpp.

	
% evaluation_value(+Plr, +B, ?Val)
% Retourne une valeur en fonction du plateau B correspondant aux "points" du joueur.
evaluation_value(Plr, B, Val) :-
	row(B, 1, R1), points(R1, Plr, B, PtsR1),
	row(B, 2, R2), points(R2, Plr, B, PtsR2),
	row(B, 3, R3), points(R3, Plr, B, PtsR3),
	column(B, 1, C1), points(C1, Plr, B, PtsC1),
	column(B, 2, C2), points(C2, Plr, B, PtsC2),
	column(B, 3, C3), points(C3, Plr, B, PtsC3),
	diagonal(B, 1, D1), points(D1, Plr, B, PtsD1),
	diagonal(B, 2, D2), points(D2, Plr, B, PtsD2),
	Val is PtsR1 + PtsR2 + PtsR3 + PtsC1 + PtsC2 + PtsC3 + PtsD1 + PtsD2.
	
	
% points(+L, +Plr, +B, ?Pts)
% Compte le nombre de pions du joueur Plr dans une liste et attribue des points
% en fonction de ce nombre.
points(L, Plr, B, Pts) :-
	include(=:=(Plr), L, PPlr),
	length(PPlr, X),
	pts_value(X, Pts).

	
% pts_value(+X, ?Pts)
% Points attribués pour X pions alignés.	
pts_value(0, 0).
pts_value(1, 1).
pts_value(2, 10).
pts_value(3, 100).


% alpha_beta(+Plr, +B, +Depth, +Alpha, +Beta, ?Move, ?Value)
% Algorithme d'élagage alpha-beta, depth représentant la profondeur 
% de l'algorithme et value le score évalué pour un plateau donné.
alpha_beta(Plr, B, 0, Alpha, Beta, Move, Value) :-
	evaluation_board(B, Plr, Value).
%alpha_beta(Plr, B, _, _, _, _, 100) :-
%	evaluation_board(B, Plr, 100).
%alpha_beta(Plr, B, _, _, _, _, -100) :-
%	evaluation_board(B, Plr, -100).
alpha_beta(Plr, B, Depth, Alpha, Beta, Move, Value) :-
	findall(X, move(Plr, B, X, _), Moves), 
	%removeOldMove(OldMove, Moves),
	Alpha1 is -Beta,
	Beta1 is -Alpha,
	%write('alpha_beta / Depth = '), write(Depth), nl,
	find_best(Plr, B, Depth, Alpha1, Beta1, Moves, Move).


% find_best(+Plr, +B, +Depth, +Alpha, +Beta, +Moves, ?BestMove)
% Trouve le meilleur coup à jouer.	
find_best(Plr, B, Depth, Alpha, Beta, [Move|RMoves], BestMove) :-
	move(Plr, B, Move, NewB),
	get_opponent(Plr, Opp),
	%writeln('-----------'),
	%writeln(NewB),
    %write('Alpha : '), write(Alpha), nl,
    %write('Beta : '), write(Beta), nl,
	Depth1 is Depth-1,
	alpha_beta(Opp, NewB, Depth1, Alpha, Beta, Move, Value),
	Value1 is -Value,
	alpha_test(Plr, B, Depth, Alpha, Beta, Move, RMoves, BestMove, Value1).
	
% Effectue l'élagage de l'arbre en fonction d'alpha et beta (et de la valeur 
% de score obtenue). On arrête la recherche lorsque Alpha >= Beta, lorsque
% Value >= Alpha on appelle find_best avec Alpha = Value et on remplace BestMove
% par le move actuel. Dans les autres cas on appelle juste find_best avec 
% les mêmes paramètres qu avant. 
alpha_test(_, _, _, Alpha, Beta,_, _, _, _):-
	Alpha >= Beta,!.
	
alpha_test(Plr, B, Depth, Alpha, Beta, Move, [], Move, Value):-
	Value >= Alpha,!.
alpha_test(Plr, B, Depth, Alpha, Beta, _, [], BestMove, Value).
	
alpha_test(Plr, B, Depth, Alpha, Beta, Move, RMoves, Move, Value):-
	Value >= Alpha,!,
	find_best(Plr, B, Depth, Value, Beta, RMoves, Move).

alpha_test(Plr, B, Depth, Alpha, Beta, _, RMoves, BestMove, Value):-
	find_best(Plr, B, Depth, Alpha, Beta, RMoves, BestMove).
