:- module(mod_interface, [init_ui/0]).
:- use_module('regles_jeu.pl').

% Permet de sauvegarder le board
:- dynamic board/1.

% Initialise l'interface utilisateur
init_ui:-
    retractall(board(_)),
    init_board(B),
    assert(board(B)).
    
% Initialise le plateau de jeu
init_board([0, 0, 0, 0, -1, 0, 0, 0, 0]).

start(0) :-
    writeln('Niveau de difficulté de l'IA :'),
    level(LVL),
    is_first(Player),
    init_board(B),
    Player == 1 -> play(B, LVL, Player);
    Player == 2 -> playIA(B, LVL, Player);
    .

start(1) :- 
    writeln('Niveau de difficulté de l'IA 1 :'),
    level(LVL1),
    writeln('Niveau de difficulté de l'IA 2 :'),
    level(LVL2),
    init_board(B),
    playIAvsIA(B, LVL1, LVL2).

start(_) :- !.

% Demande la difficulté de l'IA au joueur
level(LVL) :-
    writeln('\t0.\tFacile'),
    writeln('\t1.\tMoyen'),
    writeln('\t2.\tDifficile'),
    read(LVL),
    integer(LVL),
    between(0, 2, LVL), !.

level(LVL) :-
    writeln('Le choix doit être 0, 1 ou 2 !'),
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
    writeln('Le choix doit être 1 ou 2 !'),
    is_first(

play(B, LVL, Player) :-
    board(B),
    choice_action(B, Move, Player),
    save(Player, Move),
    board(NewB),
    print_board(Player, LVL, NewB),
    not(won),
    get_opponent(Player, Opponent),
    playIA(B, LVL, Opponent).
  
playIA(B, LVL, Player) :-
    board(B1),
    Depth is LVL + 3,
    alpha_beta(Player, Depth, B1, -10000, 10000, Move, B, Value), !,
    save(Player, Move),
    board(NewB),
    print_board(Player, LVL, NewB),
    not(won)
    get_opponent(Player, Opponent),
    play(B1, LVL, Opponent).
    
playIAvsIA(LVL1, LVL2) :-
    Depth1 is LVL1 + 3,
    Depth2 is LVL2 + 3,
    playIAvsIA(LVL1, LVL2, Depth1, Depth2, [-1], [-1]), % à revoir
    
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
  
won :-
    board(B),
    win(PLayer, B),
    writef("Le joueur %w a gagné !", [Player]),
    init_ui, !.
  
