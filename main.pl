:- use_module('interface.pl').

% Crédits
credit :- 
  writeln('-------------------------------------'),
  writeln('\tProgramme écrit par'),
  writeln('\t Florian RIFFLART'),
  writeln('\t  Pierrick GRAF'),
  writeln('-------------------------------------'), nl, nl.
  

% Menu de sélection
main :-
  credit,
  init_ui,
  writeln('-------------------------------------'),
  writeln('\t0 - Joueur Vs. IA'),
  writeln('\t1 - IA Vs. IA'),
  writeln('\t2 - Joueur Vs. Joueur'),
  writeln('\t3 - Quitter'),
  writeln('-------------------------------------'),
  menu_choice(C),
  tty_clear,
  start(C).
  
menu_choice(C) :-
    read(C),
    integer(C),
    between(0, 3, C), !.  
menu_choice(C) :-
    writeln('Erreur : Le choix doit être 0, 1, 2 ou 3 !'),
    menu_choice(C).
  
  % Lance le menu
:- set_prolog_stack(global, limit(2*10**9)), main.
