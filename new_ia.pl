% TO DO
% - eval
% - alpha_beta
% - élaguer

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
	win(Plr, B).
evaluation_board(B, Plr, -100) :-
	get_opponent(Plr, Opp),
	win(Opp, B).
evalutaion_board(B, Plr, Val) :- 
	get_opponent(Plr, Opp),
	evaluation_value(Plr, B, ValPlr),
	evaluation_value(Opp, B, ValOpp),
	Val is ValPlr - ValOpp.

	
% evaluation_value(+Plr, +B, ?Val)
% Retourne une valeur en fonction du plateau B correspondant aux "points" du joueur.
evaluation_value(Plr, B, Val) :-
	foreach(row(R), points(R, Plr, B, Pts1)),
	foreach(column(C), points(C, Plr, B, Pts2)),
	foreach(diagonal(D), points(D, Plr, B, Pts3)),
	Val is Pts1 + Pts2 + Pts3.
	
	
% points(+L, +Plr, +B, ?Pts)
% Compte le nombre de pions du joueur Plr dans une liste et attribue des points
% en fonction de ce nombre.
points(L, Plr, B, Pts) :-
	board_value(L, B, BL),
	include(=:=(Plr), BL, PPlr),
	length(PPlr, X),
	pts_value(X, Pts).


% Récupère la "valeur" des éléments situés sur les indexs présents dans L du plateau.
% (par exemple si L représente la première colonne, on récupère les valeurs du 
% plateau pour les indexs 0, 3, 6). Cela permet de connaitre le nombre de pions d'un 
% joueur sur le plateau.	
board_value(L, B, [Y1, Y2, Y3]) :-
	nth0(0, L, X1),
	nth0(X1, B, Y1),
	nth0(1, L, X2),
	nth0(X2, B, Y2),
	nth0(2, L, X3),
	nth0(X3, L, Y3).

	
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
    writeln('Depth = 0'),!,
	evaluation_board(B, Plr, Value).
alpha_beta(Plr, B, _, _, _, _, 100) :-
	evaluation_board(B, Plr, 100).
alpha_beta(Plr, B, _, _, _, _, -100) :-
	evaluation_board(B, Plr, -100).
alpha_beta(Plr, B, Depth, Alpha, Beta, Move, _) :-
	findall(X, move(Plr, B, X, _), Moves), 
    Alpha1 is -Beta,
    Beta1 is -Alpha,
    write('alpha_beta / Depth = '), write(Depth), nl,
	find_best(Plr, B, Depth, Alpha1, Beta1, Moves, Move).


% find_best(+Plr, +B, +Depth, +Alpha, +Beta, +Moves, ?BestMove)
% Trouve le meilleur coup à jouer.	
find_best(_, _, _, _, _, [], _).	
find_best(Plr, B, Depth, Alpha, Beta, [Move|RMoves], BestMove) :-
	move(Plr, B, Move, NewB),
    writeln('find_best'),
    Depth1 is Depth-1,
	alpha_beta(Opp, NewB, Depth1, Alpha, Beta, Move, Value),
    Value1 is -Value,
	alpha_test(Plr, B, Depth1, Alpha, Beta, [Move|RMoves], BestMove, Value1).
	
	
% Effectue l'élagage de l'arbre en fonction d'alpha et beta (et de la valeur 
% de score obtenue). On arrête la recherche lorsque Alpha >= Beta, lorsque
% Value >= Alpha on appelle find_best avec Alpha = Value et on remplace BestMove
% par le move actuel. Dans les autres cas on appelle juste find_best avec 
% les mêmes paramètres qu avant. 
alpha_test(_, _, _, Alpha, Beta, _, _, _):-
    writeln('alpha_test 1'),
	Alpha >= Beta.
alpha_test(Plr, B, Depth, Alpha, Beta, [Move|RMoves], Move, Value):-
    writeln('alpha_test 2'),
	Value >= Alpha,
	find_best(Plr, B, Depth, Value, Beta, RMoves, Move).
alpha_test(Plr, B, Depth, Alpha, Beta, [_|RMoves], BestMove, _):-
    writeln('alpha_test 3'),
	find_best(Plr, B, Depth, Alpha, Beta, RMoves, BestMove).
