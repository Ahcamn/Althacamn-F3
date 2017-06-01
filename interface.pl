:- module(mod_interface, [init_ui/0]).
:- use_module('regles_jeu.pl').

% Permet de sauvegarder le board
:- dynamic board/1.

% Initialise l'interface utilisateur
init_ui:-
    retractall(board(_)),
    init_board(B),
    assert(board(B)).
    
% Initialise le board
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
  choice_action(B, Coup, Player),
  save(Player, Coup),
  board(NewB),
  print_board(Player, LVL, NewB),
  not(won),
  get_opponent(Player, Opponent),
  playIA(B, LVL, Opponent).
  
won :-
  board(B),
  win(PLayer, B),
  writef("Le joueur %w a gagné !", [Player]),
  init_ui, !.
  
