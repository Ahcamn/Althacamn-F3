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
	row(B, 1, R1), points(R1, Plr, PtsR1),
	row(B, 2, R2), points(R2, Plr, PtsR2),
	row(B, 3, R3), points(R3, Plr, PtsR3),
	column(B, 1, C1), points(C1, Plr, PtsC1),
	column(B, 2, C2), points(C2, Plr, PtsC2),
	column(B, 3, C3), points(C3, Plr, PtsC3),
	diagonal(B, 1, D1), points(D1, Plr, PtsD1),
	diagonal(B, 2, D2), points(D2, Plr, PtsD2),
	Val is PtsR1 + PtsR2 + PtsR3 + PtsC1 + PtsC2 + PtsC3 + PtsD1 + PtsD2.
	
	
% points(+L, +Plr, ?Pts)
% Compte le nombre de pions du joueur Plr dans une liste et attribue des points
% en fonction de ce nombre.
points(L, Plr, Pts) :-
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
alpha_beta(Plr, B, 0, _, _, _, Value) :-
    writeln('Depth = 0'),
	evaluation_board(B, Plr, Value).
%alpha_beta(Plr, B, _, _, _, _, 100) :-
%	evaluation_board(B, Plr, 100).
%alpha_beta(Plr, B, _, _, _, _, -100) :-
%	evaluation_board(B, Plr, -100).
alpha_beta(Plr, B, Depth, Alpha, Beta, Move, BestValue) :-
	findall(X, move(Plr, B, X, _), Moves), 
	Alpha1 is -Beta,
	Beta1 is -Alpha,
    Depth1 is Depth-1,
	write('alpha_beta / Depth = '), write(Depth), nl,
	find_best(Plr, B, Depth1, Alpha1, Beta1, Moves, BestMove, BestValue),
	write('value dans alpha beta : '), write(BestValue), nl,
	write('best move dans alpha beta : '), write(BestMove), nl.


% find_best(+Plr, +B, +Depth, +Alpha, +Beta, +Moves, ?BestMove, ?BestValue)
% Trouve le meilleur coup à jouer.	
find_best(_, _, _, _, _, [], _, _) :- !.
find_best(Plr, B, Depth, Alpha, Beta, [Move|RMoves], BestMove, BestValue) :-
	move(Plr, B, Move, NewB),
	get_opponent(Plr, Opp),
	writeln('-----------'),
	writeln(Move),
	alpha_beta(Opp, NewB, Depth, Alpha, Beta, Move, Value),
	write('Depth : '), write(Depth), nl,
    write('Alpha : '), write(Alpha), nl,
    write('Beta : '), write(Beta), nl,
	Value1 is -Value,
	write('Value : '), write(Value1), nl,
	alpha_test(Plr, B, Depth, Alpha, Beta, Move, RMoves, BestMove, BestValue, Value1),
	write('best value : '), write(BestValue), nl.
	
% Effectue l'élagage de l'arbre en fonction d'alpha et beta (et de la valeur 
% de score obtenue). On arrête la recherche lorsque Alpha >= Beta, lorsque
% Value >= Alpha on appelle find_best avec Alpha = Value et on remplace BestMove
% par le move actuel. Dans les autres cas on appelle juste find_best avec 
% les mêmes paramètres qu avant. 
%alpha_test(_,_,_,_,_,_,[],_,_,_):- !, writeln('liste vide').

alpha_test(_, _, _, Alpha, Beta,_, _, _, _, _):-
	Alpha >= Beta, !, writeln('Alpha >= Beta').
	
alpha_test(Plr, B, Depth, Alpha, Beta, Move, RMoves, _, _, Value) :-
	Value >= Alpha, !, writeln('Value >= Alpha, appel de find_best'),
	find_best(Plr, B, Depth, Value, Beta, RMoves, Move, Value).

alpha_test(Plr, B, Depth, Alpha, Beta, _, RMoves, BestMove, BestValue, _):-
	writeln('appel de find_best'),
	find_best(Plr, B, Depth, Alpha, Beta, RMoves, BestMove, BestValue).
	
add_to_list(X, L, [X|L]).
