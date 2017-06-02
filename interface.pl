% TO DO : 
% - get_empty_tile(C1) dans choice_action
% - modifier la fonction pour la difficulté du jeu

:- module(mod_interface, [init_board/1, init_ui/0, scan_choice/1, start/1]).
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
    writeln("Niveau de difficulté de l'IA :"),
    level(LVL),
    is_first(Player),
    init_board(B),
    Player == 1 -> play(B, LVL, Player);
    Player == 2 -> playIA(B, LVL, Player);
    nl.
start(1) :- 
    writeln("Niveau de difficulté de l'IA 1 :"),
    level(LVL1),
    writeln("Niveau de difficulté de l'IA 2 :"),
    level(LVL2),
    init_board(B),
    playIAvsIA(LVL1, LVL2).
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
    writeln('Voulez-vous commencer en 1er ?'),
    writeln('\t1. Oui'),
    writeln('\t2. Non'),
    read(Player),
    integer(Player),
    between(1, 2, Player), !.
is_first(Player) :-
    writeln('Erreur : Le choix doit être 1 ou 2 !'),
    is_first(Player).

% Tour du joueur
play(B, LVL, Player) :-
    board(B),
    choice_action(B, Move, Player),
    save(Player, Move),
    board(NewB),
    print_board(Player, LVL, NewB),
    not(won),
    get_opponent(Player, Opponent),
    playIA(B, LVL, Opponent).
  
% Tour de l'IA
playIA(B, LVL, Player) :-
    board(B1),
    Depth is LVL + 3,
    alpha_beta(Player, Depth, B1, -10000, 10000, Move, B, Value), !,
    save(Player, Move),
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
    save(1, Move),
    board(NewB),
    print_board(1, LVL1, NewB),
    not(won),
    alpha_beta(2, Depth2, NewB, -10000, 10000, Move2, P2, Value), !,
    save(2, Move2),
    board(NewB2),
    print_board(2, LVL2, NewB2),
    not(won),
    playIAvsIA(LVL1, LVL2, Depth1, Depth2, B, NewB).
  
% Permet de définir un coup
choice_action(B, [C1, C2, C], Player) :-
    writeln('Choisissez une action :'),
    writeln('\t0. Poser un pion'),
    writeln('\t1. Déplacer un pion'),
    writeln('\t2. Déplacer la case vide'),
    scan_choice(C),
    C == 0 -> C1 is -1, scan_destination(C2);
    C == 2 -> scan_origine(C1), scan_destination(C2), is_move_allowed(C2, C1, C);
    scan_origine(C1), scan_destination(C2), nth0(C1, B, Player), !.
choice_action(B, Move, Player) :-
    writeln("Erreur : l'action doit être 0, 1 ou 2 !"),
    choice_action(B, Move, Player).
    
% Demande au joueur de choisir une action
scan_choice(C) :-
    read(C),
    integer(C),
    between(0, 2, C), !.  
scan_choice(C) :-
    writeln('Erreur : Le choix doit être 0, 1 ou 2 !'),
    scan_choice(C).
    
% Demande au joueur de choisir l'emplacement d'origine
scan_origine(C) :-
    writeln('Emplactement d'origine :'),
    read(C),
    integer(C),
    between(0, 8, C), !.
scan_origine(C) :-
    writeln('Erreur : Le choix doit être entre 0 et 8 inclus !'),
    scan_choice(C).

% Demande au joueur de choisir la destination
scan_destination(C) :-
    writeln('Destination :'),
    read(C),
    integer(C),
    between(0, 8, C), !.
scan_destination(C) :-
    writeln('Erreur : Le choix doit être entre 0 et 8 inclus !'),
    scan_destination(C).
  
% Vérifie si un joueur à gagné la partie 
won :-
    board(B),
    win(Player, B),
    writef("Le joueur %w a gagné !", [Player]),
    init_ui, !.
   
save(Player,Move) :-
    retract(board(B)),
    move(Player,B,Move,B1),
    assert(board(B1)).
    
% Permet de savoir si le déplacement de case choisi est possible
is_move_allowed(0, C, 2) :-
    member(C, [1, 3]).
is_move_allowed(1, C, 2) :-
    member(C, [0, 2, 4]).
is_move_allowed(2, C, 2) :-
    member(C, [1, 5]).
is_move_allowed(3, C, 2) :-
    member(C, [0, 4, 6]).
is_move_allowed(4, C, 2) :-
    member(C, [1, 5, 7, 3]).
is_move_allowed(5, C, 2) :-
    member(C, [2, 8, 4]).
is_move_allowed(6, C, 2) :-
    member(C, [3, 7]).
is_move_allowed(7, C, 2) :-
    member(C, [4, 8, 6]).
is_move_allowed(8, C, 2) :-
    member(C, [5, 7]).

is_move_allowed(0, C, 3) :-
    member(C, [6, 2]).
is_move_allowed(1, 7, 3).
is_move_allowed(2, C, 3) :-
    member(C, [0, 8]).
is_move_allowed(3, 5, 3).
is_move_allowed(5, 3, 3).
is_move_allowed(6, C, 3) :-
    member(C, [0, 8]).
is_move_allowed(7, 1, 3).
is_move_allowed(8, C, 3) :-
    member(C, [2, 6]).

% Affiche le plateau de jeu
print_board(Player, LVL, [C1, C2, C3, C4, C5, C6, C7, C8, C9]):-
    LVL \= -1, !,
    getLevel(LVL, Level),
    write('     _____'), nl,
    write('    |'), token(C1), write(' '), token(C2), write(' '), token(C3), write('|'), nl,
    write('    |'), token(C4), write(' '), token(C5), write(' '), token(C6), write('|'),
    write('\tPlayer '), write(Player), write(' (IA) '), write(Level), nl,
    write('    |'), token(C7), write(' '), token(C8), write(' '), token(C9), write('|'), nl,
    write('     -----'), nl, nl.

print_board(Player, -1, [C1, C2, C3, C4, C5, C6, C7, C8, C9]):-
    !,
    write('     _____'), nl,
    write('    |'), token(C1), write(' '), token(C2), write(' '), token(C3), write('|'), nl,
    write('\tPlayer '), write(Player), write(' (Joueur) '), nl,
    write('  |'), token(C4), write(' '), token(C5), write(' '), token(C6), write('|\tJoueur'), nl,
    write('    |'), token(C7), write(' '), token(C8), write(' '), token(C9), write('|'), nl,
    write('     -----'), nl, nl.
    
% Affecte un caractère à chaque cases du plateau de jeu en fonction de leur id
token(0) :-
    write(' ').
token(-1) :-
    write('').
token(1) :-
    write('o').
token(2):-
    write('x').
