% unsafe(NumHens, NumFoxes) determines if it is a safe move...
unsafe(H, F) :- H =< F.
safe(H, F) :- not(unsafe(H, F)).

% left to right crossings
cross([H, F, 0], [H2, F2, 1]) :-
  H2 =:= (H - 1),
  H > 0.

cross([H, F, 0], [H2, F2, 1]) :-
  H2 =:= (H - 2),
  H > 1.

cross([H, F, 0], [H2, F2, 1]) :-
  H2 =:= (H - 1),
  F2 =:= (F - 1),
  H > 0,
  F > 0.

cross([H, F, 0], [H2, F2, 1]) :-
  F2 =:= (F - 1),
  F > 0.

cross([H, F, 0], [H2, F2, 1]) :-
  F2 =:= (F - 2),
  F > 1.

% right to left crossings
cross([H, F, 1], [H2, F2, 0]) :-
cross([H, F, 1], [H2, F2, 0]) :-
cross([H, F, 1], [H2, F2, 0]) :-
cross([H, F, 1], [H2, F2, 0]) :-
cross([H, F, 1], [H2, F2, 0]) :-
