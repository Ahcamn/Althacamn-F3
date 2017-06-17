:- module(mod_ia, [evaluation_board/3, min_max/5]).
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

% delMove(+Moves, +Move, ?NewMoves)
% Renvoi NewMoves qui correspond à la liste Moves sans l'élément Move
delMove([], _, []).
delMove([X|R1], X, R1) :- !.
delMove([Y|R1], X, [Y|R3]) :- delMove(R1, X, R3).

min_max(Plr, B, Depth, PrevMove, Move) :- 
    min_max(Plr, B, Depth, Value, PrevMove, Move).

% min_max(+Plr, +B, +Depth, ?Value, +PrevMove, ?Move) 
% Algorithme Min Max qui appelle l'évaluation du plateau ou find_best.
% Plr correspond au joueur, B au plateau, Depth à la profondeur de l'algorithme
% Value la valeur retournée par min_max, PrevMove le déplacement précédent
% utilisé dans findall/7 et Move correspond au meilleur coup à effectuer.
min_max(Plr, B,  0, Value, _, _) :-
    !, evaluation_board(B, Plr, Value).
min_max(Plr, B, Depth, Value, PrevMove, Move) :-
    Depth1 is Depth - 1,
    findall(X, move(Plr, B, X, _), Moves),
    find_best(Plr, B, Depth1, Value, PrevMove, Move, Moves).

% find_best(+Plr, +B, +Depth, ?Value, +PrevMove, ?Move, ?Moves)
% Deux prédicats find_best/7 qui appellent le prédicat find_best/9 selon PrevMove :
% soit PrevMove était un déplacement de case vide et il faut le supprimer de la liste 
% des Moves, dans ce cas on met la nouvelle liste sans PrevMove en entrée dans find_best/9
% ou il n'y a pas besoin de supprimer le PrevMove de la liste et on appelle juste find_best/9.
find_best(Plr, B, Depth, Value, [TS, TE, I], Move, Moves) :-  
    between(2,3,I),
    delMove(Moves, [TE, TS, I], NewMoves),
    find_best(Plr, B, Depth, NewMoves, -1000, nil, Value, [TS, TE, I], Move), !.
find_best(Plr, B, Depth, Value, PrevMove, Move, Moves) :-
    find_best(Plr, B, Depth, Moves, -1000, nil, Value, PrevMove, Move).
    

% find_best(+Plr, +B, +Depth, +Moves, +Value0, +Move0, ?BestValue, +PrevMove, ?BestMove)
% Trouve le meilleur coup à jouer.	 
% Plr correspond au joueur, B au plateau, Depth à la profondeur, Moves au tableau de coups
% déterminé en min_max, Value0 correspond à la meilleur valeur actuelle, Move0 correspond au
% meilleur coup actuel, BestValue est la valeur qui sera retournée en fin d'exécution, BestMove
% est le meilleur coup recherché, et PrevMove correspond au déplacement précédent. 
% Une fois arrivé en fin de liste de déplacements, la Value0 actuelle correspond à la meilleure
% value BestValue et Move0 correspond au meilleur déplacement qui était dans la liste initiale Moves.
find_best(_, _, _, [], Value, Move, Value, _, Move).
find_best(Plr, B, Depth, [Move|Moves], Value0, Move0, BestValue, PrevMove, BestMove) :-
    move(Plr, B, Move, NewB),
    get_opponent(Plr, Opp),
    min_max(Opp, NewB, Depth, OppValue, PrevMove, _OppMove),
    Value is -OppValue,
	% Si Value > Value0, on remplace Value0 par Value et Move0 par Move
    Value > Value0 -> find_best(Plr, B, Depth, Moves, Value, Move, BestValue, PrevMove, BestMove); 
	% Sinon on ne change rien
    find_best(Plr, B, Depth, Moves, Value0, Move0, BestValue, PrevMove, BestMove).
