% riddle start and end states
start_state(state(3, 3, 0)).
end_state(state(0, 0, 1)).

% helper function
printl(String) :- write(String), nl.

% overarching function to solve riddle
solve :-
  printl('starting?'),
  bfs([[start_state(X)]], Problem),
  printl(Problem).

% base case for bfs... 
bfs([Node | _], Node) :-
  end_state(Node).

% recursive part of bfs...
bfs(Crossings, [Node | Tree]) :-
  progress_tree(Node, A),
  append(Tree, A, B),
  bfs(B, Crossings).

progress_tree([Node | Tree], NewProgress) :-
  setof([New, Node | Tree], (cross(Node, New), (member(New, [Node | Tree]), !)), NewProgress), !.

progress_tree(_, []).

% all possible river crossings, one hen states are removed as they will not occur

% left to right crossings
cross(state(H1, F1, 0), state(H2, F2, 1)) :-
  printl('Two hens cross L -> R'),
  H2 is (H1 - 2),
  H1 > 1,
  valid(H2, F1).

cross(state(H1, F1, 0), state(H2, F2, 1)) :-
  printl('One hen and one fox cross L -> R'),
  H2 is (H1 - 1),
  F2 is (F1 - 1),
  H1 > 0,
  F1 > 0,
  valid(H2, F2).

cross(state(H1, F1, 0), state(H2, F2, 1)) :-
  printl('One fox crosses L -> R'),
  F2 is (F1 - 1),
  F1 > 0,
  valid(H1, F2).

cross(state(H1, F1, 0), state(H2, F2, 1)) :-
  printl('Two foxes cross L -> R'),
  F2 is (F1 - 2),
  F1 > 1,
  valid(H1, F2).

% right to left crossings
cross(state(H1, F1, 1), state(H2, F2, 0)) :-
  printl('Two hens cross L <- R'),
  H2 is (H1 + 2),
  H1 < 2,
  valid(H2, F1).

cross(state(H1, F1, 1), state(H2, F2, 0)) :-
  printl('One hen and one fox crosses L <- R'),
  F2 is F1 + 1,
  H2 is H1 + 1,
  F1 < 3,
  H1 < 3,
  valid(H2, F2).

cross(state(H1, F1, 1), state(H2, F2, 0)) :-
  printl('One fox crosses L <- R'),
  F2 is (F1 + 1),
  F1 < 3,
  valid(H1, F2).

cross(state(H1, F1, 1), state(H2, F2, 0)) :-
  printl('Two foxes cross L <- R'),
  F2 is (F1 + 2),
  F1 < 2,
  valid(H1, F2).

valid(H, F) :- 
  (F =< H; H =:= 0).
