% TO DO :
% - Crédits

:- use_module('interface.pl').

% Menu de sélection
main :-
  init_ui,
  writeln('----------- Menu ------------'),
  writeln('\t0 - Joueur Vs. IA'),
  writeln('\t1 - IA Vs. IA'),
  writeln('\t2 - Joueur Vs. Joueur'),
  writeln('\t3 - Quitter'),
  scan_choice(C),
  tty_clear,
  start(C).
  
  % Lance le menu
:- set_prolog_stack(global, limit(2*10**9)), main.
