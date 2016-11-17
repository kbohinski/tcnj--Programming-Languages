solve :-
  write('starting?'),
  cross_river([3, 3, 0], [0, 0, 1], [[3, 3, 0]]).

cross_river([H, F, LR], [H, F, LR]) :-
  write('base case?').

cross_river([H, F, LR], [H1, F1, LR1], past_crossings) :-
  cross([H, F, LR], [A, B, C]),
  safe(A, B),
  not(member([A, B, C], past_crossings)),
  cross_river([A, B, C], [H1, F1, LR1], [[A, B, C] | past_crossings]).

% unsafe(NumHens, NumFoxes) determines if it is a safe move...
unsafe(H, F) :- H < F; H =:= 0.
safe(H, F) :- not(unsafe(H, F)).

% left to right crossings
cross([H, F, 0], [H2, F2, 1]) :-
  write('One hen crosses L -> R'),
  H2 =:= (H - 1),
  H > 0.

cross([H, F, 0], [H2, F2, 1]) :-
  write('Two hens cross L -> R'),
  H2 =:= (H - 2),
  H > 1.

cross([H, F, 0], [H2, F2, 1]) :-
  write('One hen and one fox cross L -> R'),
  H2 =:= (H - 1),
  F2 =:= (F - 1),
  H > 0,
  F > 0.

cross([H, F, 0], [H2, F2, 1]) :-
  write('One fox crosses L -> R'),
  F2 =:= (F - 1),
  F > 0.

cross([H, F, 0], [H2, F2, 1]) :-
  write('Two foxes cross L -> R'),
  F2 =:= (F - 2),
  F > 1.

% right to left crossings
cross([H, F, 1], [H2, F2, 0]) :-
  write('One hen crosses L <- R'),
  H2 =:= (H + 1),
  H < 3.

cross([H, F, 1], [H2, F2, 0]) :-
  write('Two hens cross L <- R'),
  H2 =:= (H + 2),
  H < 2.

cross([H, F, 1], [H2, F2, 0]) :-
  write('One hen and one fox crosses L <- R'),
  F2 =:= F + 1,
  H2 =:= H + 1,
  F < 3,
  H < 3.

cross([H, F, 1], [H2, F2, 0]) :-
  write('One fox crosses L <- R'),
  F2 =:= (F + 1),
  F < 3.

cross([H, F, 1], [H2, F2, 0]) :-
  write('Two foxes cross L <- R'),
  F2 =:= (F + 2),
  F < 2.
