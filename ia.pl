% TO DO
% - eval
% - alpha_beta
% - élaguer

:- module(mod_ia, [eval_bord/2, alpha_beta/8]).
:- use_module('regles_jeu.pl').
:- use_module(library(lists)).


% ----------------- à refaire -----------------

% eval_nb_move(+J, +P, ?NbM)
% retourne le nombre de coup NbC du joueur J pour le plateau P
eval_nb_move(J, P, NbM) :-
    findall(_, deplacement(J, P, _), ListeMove),
    length(ListeMove, NbM).


% eval_bord(+P, ?Value)
% Permet de déterminer l'avantage d'un des deux joueurs.
% Si la valeur de retour est positif, alors le joueur 1 à l'avantage. Et vise versa.
% La valeur maximale est abs(100).
eval_bord(P, 100) :-
    win(1, P), !.
eval_bord(P, -100) :-
    win(2, P), !.
eval_bord(P, V) :-
    eval_value(1, P, ValueJ1),
    eval_value(2, P, ValueJ2),
    V is ValueJ1 - ValueJ2.


% eval_value(+J, +P, ?Value)
% Retourne la valeur pour le joueur J
eval_value(J, P, Value) :-
    maplist(row(P), [1, 2, 3], Rows),
    maplist(compute_points(J), Rows, PRows),
    sumlist(PRows, PtsRows),
    maplist(column(P), [1, 2, 3], Cols),
    maplist(compute_points(J), Cols, PCols),
    sumlist(PCols, PtsCols),
    diagonal(P, 1, Prof1), compute_points(J, Prof1, PtsProf1),
    diagonal(P, 2, Prof2), compute_points(J, Prof2, PtsProf2),
    Value is PtsRows + PtsCols + PtsProf1 + PtsProf2.


% compute_points(+J, +List, ?Score)
% Distribut un score en fonction du nombre de pionts allignés.
compute_points(J, List, Score) :-
    include(=:=(J), List, Filtered),
    length(Filtered, Len),
    score(Len, Score).


% score(+NbPions, ?score)
% Retourne le score associé au nombre de pionts alignés.
score(0, 0) :- !.
score(1, 1) :- !.
score(2, 5) :- !.
score(3, 100).


% alpha_beta(+J, +Depth, +P, +Alpha, +Beta, ?Move, +ForbidP, ?Value)
% Algorithme d'élagage utilisant la méthode alpha-beta.
% Depth est la profondeur de recherche avec Value la valeur du plateau
% lorsque le coup est joué.
alpha_beta(_J, 0, P, _Alpha, _Beta, _Move, _ForbidP, Value) :-
    !, eval_bord(P, Value).

alpha_beta(J, Depth, P, Alpha, Beta, Move, P, Value) :-
    !, findall(X, move(J, P, X, _), Moves),
    Alpha1 is -Beta, % max/min
    Beta1 is -Alpha,
    find_best(J, Moves, P, Depth, Alpha1, Beta1, nil, P, (Move, Value)).

alpha_beta(J, Depth, P, Alpha, Beta, Move, ForbidP, Value) :-
    findall(X, move(J, P, X, _), Moves),
    Alpha1 is -Beta, % max/min
    Beta1 is -Alpha,
    Depth1 is Depth - 1,
    find_best(J, Moves, P, Depth1, Alpha1, Beta1, nil, ForbidP, (Move, Value)).

% find_best(+J,+Moves,+P,+Depth,+Alpha,+Beta,+R,?BestMove)
% Retourne le meilleur coup à jouer.
find_best(_J, [], _P, _Depth, Alpha, _Beta, Move, _, (Move,Alpha)) :- !.
find_best(J, [Move|Moves], P, Depth, Alpha, Beta, R, ForbidP, BestMove) :-
    move(J, P, Move, NP),
    get_opponent(J, OtherJR),
    alpha_beta(OtherJR, Depth, NP, Alpha, Beta, _OtherCoup, ForbidP, Value),
    Value1 is -Value,
    pruning(J,Move, ForbidP, Value1,Depth,Alpha,Beta,Moves,P,R,BestMove).


% pruning(+J,+Move,+Value,+Depth,+Alpha,+Beta,+Moves,+P,+_R,+BestMove)
% Permet d'élaguer l'abre de recherche en fonction d'alpha et beta.
% La recherche s'arrête dans une branche lorsqu'elle sort des bornes.
pruning(J,Move, ForbidP, Value,Depth,Alpha,Beta,Moves,P,_R,BestMove) :-
    Alpha < Value,
    Value < Beta, !,
    find_best(J,Moves,P,Depth,Value,Beta,Move, ForbidP, BestMove).
pruning(J,_Move, ForbidP, Value,Depth,Alpha,Beta,Moves,P,R,BestMove) :-
    Value =< Alpha, !,
    find_best(J,Moves,P,Depth,Alpha,Beta,R, ForbidP, BestMove).
pruning(_J, Move, _, Value, _Depth, _Alpha, _Beta, _Moves, _P, _R, (Move, Value)).
