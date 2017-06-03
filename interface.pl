% TO DO : 
% - modifier la fonction pour la difficulté du jeu
% - vérifier si le joueur a 3 pions avant de demander l'emplacement 

:- module(mod_interface, [init_board/1, init_ui/0, scan_choice/1, start/1, move/4]).
:- use_module('regles_jeu.pl').
:- use_module('ia.pl').

% Permet de sauvegarder le board
:- dynamic board/1.

% Initialise l'interface utilisateur
init_ui:-
    retractall(board(_)),
    init_board(B),
    assert(board(B)).
    
% Initialise le plateau de jeu
init_board([0, 0, 0, 0, -1, 0, 0, 0, 0]).

% Lance la partie en fonction du mode de jeu choisi
start(0) :-
    writeln('Niveau de difficulté de l\'IA :'),
    level(LVL),
    is_first(Player),
    init_board(B),
    Player == 1 -> play(B, LVL, Player);
    Player == 2 -> playIA(B, LVL, Player);
    nl.
start(1) :- 
    writeln('Niveau de difficulté de l\'IA 1 :'),
    level(LVL1),
    writeln('Niveau de difficulté de l\'IA 2 :'),
    level(LVL2),
    init_board(_),
    playIAvsIA(LVL1, LVL2).
start(2) :-
    init_board(B),
    playPvP(B, LVL, 1).
start(_) :- !.

% Demande la difficulté de l'IA au joueur
level(LVL) :-
    writeln('\t0. Facile'),
    writeln('\t1. Moyen'),
    writeln('\t2. Difficile'),
    read(LVL),
    integer(LVL),
    between(0, 2, LVL), !.
level(LVL) :-
    writeln('Erreur : Le choix doit être 0, 1 ou 2 !'),
    level(LVL).
  
% Demande au joueur si il veut commencer en 1er le jeu
is_first(Player) :-
    nl, writeln('Voulez-vous commencer en 1er ?'),
    writeln('\t1. Oui'),
    writeln('\t2. Non'),
    read(Player),
    integer(Player),
    between(1, 2, Player), !.
is_first(Player) :-
    writeln('Erreur : Le choix doit être 1 ou 2 !'),
    is_first(Player).

% Mode Joueur contre Joueur
playPvP(B1, LVL, Player) :-
    board(B),
    nl, writef("Choisissez une action (Joueur %w) :",[Player]), nl,
    writeln('\t0. Poser un pion'),
    writeln('\t1. Déplacer un pion'),
    writeln('\t2. Déplacer la case vide'),
    scan_choice(C),
    action(B, [TS, TE, C], Player),
    save_move(Player, [TS, TE, C]),
    board(NewB),
    print_board(Player, -1, NewB),
    not(won),
    get_opponent(Player, Opponent),
    playPvP(B, LVL, Opponent).
    
% Tour du joueur
play(B1, LVL, Player) :-
    board(B),
    nl, writeln('Choisissez une action :'),
    writeln('\t0. Poser un pion'),
    writeln('\t1. Déplacer un pion'),
    writeln('\t2. Déplacer la case vide'),
    scan_choice(C),
    action(B, [TS, TE, C], Player),
    save_move(Player, [TS, TE, C]),
    board(NewB),
    print_board(Player, -1, NewB),
    not(won),
    get_opponent(Player, Opponent),
    playIA(B, LVL, Opponent).
    
action(B, [TS, TE, 0], Player) :-
    TS is -1, 
    scan_destination(TE),
    setP(Player, B, TE), !.
    
action(B, [TS, TE, 1], Player) :-
    scan_origin(TS), 
    scan_destination(TE),
    moveP(Player, B, [TS, TE]), !.
    
action(B, [TS, TE, 2], _) :-
    get_empty_tile(TS, B), 
    scan_destination(TE), 
    moveT(B, [TS, TE]),
    is_move_allowed(TE, TS, 2), !.
    
action(B, [TS, TE, I], Player) :-
    nl, writeln('Erreur : Action impossible !'),
    action(B, [TS, TE, I], Player).
    
save_move(Player, Move) :-
    retract(board(B)),
    move(Player, B, Move, B1),
    assert(board(B1)).

% Tour de l'IA
playIA(B, LVL, Player) :-
    board(B1),
    Depth is LVL + 3,
    alpha_beta(Player, Depth, B1, -10000, 10000, Move, B, Value), !,
    save_move(Player, Move),
    board(NewB),
    print_board(Player, LVL, NewB),
    not(won),
    get_opponent(Player, Opponent),
    play(B1, LVL, Opponent).
   
% Tour de l'IA en mode IA vs IA   
playIAvsIA(LVL1, LVL2) :-
    Depth1 is LVL1 + 3,
    Depth2 is LVL2 + 3,
    playIAvsIA(LVL1, LVL2, Depth1, Depth2, [-1], [-1]). % à revoir
playIAvsIA(LVL1, LVL2, Depth1, Depth2, P1, P2) :-
    board(B),
    alpha_beta(1, Depth1, B, -10000, 10000, Move, P1, Value), !,
    save_move(1, Move),
    board(NewB),
    print_board(1, LVL1, NewB),
    not(won),
    alpha_beta(2, Depth2, NewB, -10000, 10000, Move2, P2, Value), !,
    save_move(2, Move2),
    board(NewB2),
    print_board(2, LVL2, NewB2),
    not(won),
    playIAvsIA(LVL1, LVL2, Depth1, Depth2, B, NewB).

% trouve la case vide sur le plateau 
get_empty_tile(0, B) :- nth0(0, B, -1).
get_empty_tile(1, B) :- nth0(1, B, -1).
get_empty_tile(2, B) :- nth0(2, B, -1).
get_empty_tile(3, B) :- nth0(3, B, -1).
get_empty_tile(4, B) :- nth0(4, B, -1).
get_empty_tile(5, B) :- nth0(5, B, -1).
get_empty_tile(6, B) :- nth0(6, B, -1).
get_empty_tile(7, B) :- nth0(7, B, -1).
get_empty_tile(8, B) :- nth0(8, B, -1).


% Demande au joueur de choisir une action
scan_choice(C) :-
    read(C),
    integer(C),
    between(0, 3, C), !.  
scan_choice(C) :-
    writeln('Erreur : Le choix doit être 0, 1 ou 2 !'),
    scan_choice(C).
    
% Demande au joueur de choisir l'emplacement d'origine
scan_origin(T) :-
    nl, writeln('Emplacement d\'origine :'),
    read(T),
    integer(T),
    between(0, 8, T), !.
scan_origin(T) :-
    writeln('Erreur : Le choix doit être entre 0 et 8 inclus !'),
    scan_origin(T).

% Demande au joueur de choisir la destination
scan_destination(T) :-
    nl, writeln('Destination :'),
    read(T),
    integer(T),
    between(0, 8, T), !.
scan_destination(T) :-
    writeln('Erreur : Le choix doit être entre 0 et 8 inclus !'),
    scan_destination(T).
  
% Vérifie si un joueur à gagné la partie 
won :-
    board(B),
    win(Player, B),
    writef("Le joueur %w a gagné !", [Player]),
    init_ui, !.
    
% Permet de savoir si le déplacement de case choisi est possible
is_move_allowed(0, T, 2) :-
    member(T, [1, 3]).
is_move_allowed(1, T, 2) :-
    member(T, [0, 2, 4]).
is_move_allowed(2, T, 2) :-
    member(T, [1, 5]).
is_move_allowed(3, T, 2) :-
    member(T, [0, 4, 6]).
is_move_allowed(4, T, 2) :-
    member(T, [1, 5, 7, 3]).
is_move_allowed(5, T, 2) :-
    member(T, [2, 8, 4]).
is_move_allowed(6, T, 2) :-
    member(T, [3, 7]).
is_move_allowed(7, T, 2) :-
    member(T, [4, 8, 6]).
is_move_allowed(8, T, 2) :-
    member(T, [5, 7]).

is_move_allowed(0, T, 3) :-
    member(T, [6, 2]).
is_move_allowed(1, 7, 3).
is_move_allowed(2, T, 3) :-
    member(T, [0, 8]).
is_move_allowed(3, 5, 3).
is_move_allowed(5, 3, 3).
is_move_allowed(6, T, 3) :-
    member(T, [0, 8]).
is_move_allowed(7, 1, 3).
is_move_allowed(8, T, 3) :-
    member(T, [2, 6]).

getDifficulty(0, 'Facile').
getDifficulty(1, 'Moyen').
getDifficulty(2, 'Difficile').


%Modification de l'élément voulu du plateau
modifyBoard(Val, 0, [_|R], [Val|R]) :- !.
modifyBoard(Val, I, [X|R1], [X|R2]) :-
    I > 0, I1 is I-1, modifyBoard(Val, I1, R1, R2), !.
    
    
% Poser un pion (max 3)
move(Plr, B, [-1, T, 0], NewB) :-
    modifyBoard(Plr, T, B, NewB).
    

% Déplacer un pion (TE = case d'arrivé, TS = case de départ)
move(Plr, B, [TS, TE, 1], NewB) :-
    modifyBoard(0, TS, B, Temp),
    modifyBoard(Plr, TE, Temp, NewB).


% Déplacer une case
move(_, B, [TS, TE, 2], NewB) :-
    modifyBoard(-1, TE, B, Temp),
    nth0(TE, B, Val),
    modifyBoard(Val, TS, Temp, NewB).


% Déplacer deux cases
move(_, B, [TS, TE, 3], NewB) :-
    empty2(TS, TE, TI),
    move(_, B, [TS, TI, 2], Temp1),
    move(_, Temp1, [TI, TE, 2], NewB).
    

% Affiche le plateau de jeu
print_board(Player, LVL, [T1, T2, T3, T4, T5, T6, T7, T8, T9]):-
    LVL \= -1, !,
    getDifficulty(LVL, Level),
    write('     _____'), nl,
    write('    |'), token(T1), write(' '), token(T2), write(' '), token(T3), write('|'), nl,
    write('    |'), token(T4), write(' '), token(T5), write(' '), token(T6), write('|'),
    write('\tPlayer '), write(Player), write(' (IA) '), write(Level), nl,
    write('    |'), token(T7), write(' '), token(T8), write(' '), token(T9), write('|'), nl,
    write('     -----'), nl, nl.

print_board(Player, -1, [T1, T2, T3, T4, T5, T6, T7, T8, T9]):-
    !,
    write('     _____'), nl,
    write('    |'), token(T1), write(' '), token(T2), write(' '), token(T3), write('|'), nl,
    write('    |'), token(T4), write(' '), token(T5), write(' '), token(T6), write('|'), 
    write('\tPlayer '), write(Player), write(' (Joueur) '), nl,
    write('    |'), token(T7), write(' '), token(T8), write(' '), token(T9), write('|'), nl,
    write('     -----'), nl, nl.
    
% Affecte un caractère à chaque cases du plateau de jeu en fonction de leur id
token(0) :-
    write(' ').
token(-1) :-
    write('#').
token(1) :-
    write('o').
token(2):-
    write('x').
