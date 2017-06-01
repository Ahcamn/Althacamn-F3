

% Menu de sélection
main :-
  writeln('----------- Menu ------------'),
  writeln('0 - Joueur Vs. IA'),
  writeln('1 - IA Vs. IA'),
  writeln('2 - Quitter'),
  choix(C),
  tty_clear,
  start(C),
  
  % Lance le menu
:- set_prolog_stack(global, limit(2*10**9)), main.
