
% Permet de sauvegarder le board
:- dynamic board/1.

init_board([0, 0, 0, 0, -1, 0, 0, 0, 0]).

start(0) :- 
  level(LVL), 
  init_board(B),
  play(B, LVL).

start(1) :- 
  levelIA(LVL1, LVL2),
  init_board(B),
  playIA(B, LVL1, LVL2).

start(_) :- !.


play(B, LVL) :-
