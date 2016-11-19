% Kevin Bohinski & David Vassallo
% 11/18/16
% CSC 435 Programming Languages
%
% Project 3
% Prolog riddle
%
% kb.pl

% Module to ensure math is integer...
:- use_module(library(clpfd)).

% Overarching functions to solve riddle
solve :-
  start_state(X),
  printl(X),
  solve(X, [X]).

solve(S, Crossings) :-
  end_state(S);
  (cross(S, N), not(member(N, Crossings)), printl(Crossings), solve(N, [N, Crossings])).

% DOES NOT WORK -- Was a BFS to determine number of possible solutions per spec.
% solve_bfs :-
%   start_state(X),
%   bfs([[X]], Problem),
%   printl(Problem).
%
% Base case for bfs... 
% bfs([Node | _], Node) :-
%   done(Node).
% 
% Recursive part of bfs...
% bfs(Crossings, [Node | Tree]) :-
%   progress_tree(Node, A),
%   append(Tree, A, B),
%   bfs(B, Crossings).
% 
% progress_tree([Node | Tree], NewProgress) :-
%   % ! - Ensures it is done in reverse...
%   setof([New, Node | Tree], (cross(Node, New), (member(New, [Node | Tree]), !)), NewProgress), !.
% 
% progress_tree(_, []).
% 
% done([State | _]) :-
%   end_state(State).

% Riddle start and end states
start_state([3, 3, 0]).
end_state([0, 0, 1]).

% Helper functions
printl(String) :- write(String), nl.
print_trip_right(S, N) :- write(S), write(' -> '), write(N), nl.
print_trip_left(S, N) :- write(N), write(' <- '), write(S), nl.

% All possible river crossings rules, one hen crosses are removed as they will not occur...

% Left to right crossings
cross([H1, F1, 0], [H2, F2, 1]) :-
  % F2 =:= F1,
  H2 #= H1 - 2,
  H1 > 1,
  safe(H2, F1),
  printl('Two hens cross L -> R'),
  print_trip_right([H1, F1, 0], [H2, F2, 1]).

cross([H1, F1, 0], [H2, F2, 1]) :-
  H2 #= H1 - 1,
  F2 #= F1 - 1,
  H1 > 0,
  F1 > 0,
  safe(H2, F2),
  printl('One hen and one fox cross L -> R'),
  print_trip_right([H1, F1, 0], [H2, F2, 1]).

cross([H1, F1, 0], [H2, F2, 1]) :-
  % H1 =:= H2,
  F2 #= F1 - 1,
  F1 > 0,
  safe(H1, F2),
  printl('One fox cross L -> R'),
  print_trip_right([H1, F1, 0], [H2, F2, 1]).

cross([H1, F1, 0], [H2, F2, 1]) :-
  % H1 =:= H2,
  F2 #= F1 - 2,
  F1 > 1,
  safe(H1, F2),
  printl('Two foxes cross L -> R'),
  print_trip_right([H1, F1, 0], [H2, F2, 1]).

% Right to left crossings
cross([H1, F1, 1], [H2, F2, 0]) :-
  % F2 =:= F1,
  H2 #= H1 + 2,
  H1 < 2,
  safe(H2, F1),
  printl('Two hens cross L <- R'),
  print_trip_left([H1, F1, 0], [H2, F2, 1]).

cross([H1, F1, 1], [H2, F2, 0]) :-
  F2 #= F1 + 1,
  H2 #= H1 + 1,
  F1 < 3,
  H1 < 3,
  safe(H2, F2),
  printl('One hen and one fox cross L <- R'),
  print_trip_left([H1, F1, 0], [H2, F2, 1]).

cross([H1, F1, 1], [H2, F2, 0]) :-
  % H1 =:= H2,
  F2 #= F1 + 1,
  F1 < 3,
  safe(H1, F2),
  printl('One fox cross L <- R'),
  print_trip_left([H1, F1, 0], [H2, F2, 1]).

cross([H1, F1, 1], [H2, F2, 0]) :-
  % H1 =:= H2,
  F2 #= F1 + 2,
  F1 < 2,
  safe(H1, F2),
  printl('Two foxes cross L <- R'),
  print_trip_left([H1, F1, 0], [H2, F2, 1]).

% Safe function to make sure hens dont get eaten.
safe(H, F) :- 
  (F =< H; H =:= 0).
