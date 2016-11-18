% riddle start and end states
start_state(state(3, 3, 0)).
end_state(state(0, 0, 1)).

% overarching function to solve riddle
solve :-
  nl,
  write('starting?'),
  bfs([[3, 3, 0]], past_crossings),
  cross_river([3, 3, 0], [0, 0, 1], [[3, 3, 0]]).

% base case for bfs... 
bfs([Node | _], Node) :-
  end_state(Node).

% recursive part of bfs...
bfs([])

% unsafe(NumHens, NumFoxes) determines if it is a safe move...
safe(F, H) :- 
  F =< H; H =:= 0.

% all possible river crossings, one hen states are removed as they will not occur

% left to right crossings
cross(state(H1, F1, 0), state(H2, F2, 1)) :-
  nl,
  write('Two hens cross L -> R'),
  H2 is (H1 - 2),
  H1 > 1,
  safe(H2, F1).

cross(state(H1, F1, 0), state(H2, F2, 1)) :-
  nl,
  write('One hen and one fox cross L -> R'),
  H2 is (H1 - 1),
  F2 is (F1 - 1),
  H1 > 0,
  F1 > 0,
  safe(H2, F2).

cross(state(H1, F1, 0), state(H2, F2, 1)) :-
  nl,
  write('One fox crosses L -> R'),
  F2 is (F1 - 1),
  F1 > 0,
  safe(H1, F2).

cross(state(H1, F1, 0), state(H2, F2, 1)) :-
  nl,
  write('Two foxes cross L -> R'),
  F2 is (F1 - 2),
  F1 > 1,
  safe(H1, F2).

% right to left crossings
cross(state(H1, F1, 1), state(H2, F2, 0)) :-
  nl,
  write('Two hens cross L <- R'),
  H2 is (H1 + 2),
  H1 < 2,
  safe(H2, F1).

cross(state(H1, F1, 1), state(H2, F2, 0)) :-
  nl,
  write('One hen and one fox crosses L <- R'),
  F2 is F1 + 1,
  H2 is H1 + 1,
  F1 < 3,
  H1 < 3,
  safe(H2, F2).

cross(state(H1, F1, 1), state(H2, F2, 0)) :-
  nl,
  write('One fox crosses L <- R'),
  F2 is (F1 + 1),
  F1 < 3,
  safe(H1, F2).

cross(state(H1, F1, 1), state(H2, F2, 0)) :-
  nl,
  write('Two foxes cross L <- R'),
  F2 is (F1 + 2),
  F1 < 2,
  safe(H1, F2).
