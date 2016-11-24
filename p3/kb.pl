% Kevin Bohinski & David Vassallo
% 11/18/16
% CSC 435 Programming Languages
%
% Project 3
% Prolog riddle
%
% kb.pl

% Module to ensure math is integer...
% :- use_module(library(clpfd)).

% Overarching functions to solve riddle
solve :-
  start_state(X),
  printl(['Start state                      ', 3, 3, 1, 0, 0]),
  solve(X, [X], -1, []).

solve(S, Crossings, Last, Output) :-
  (end_state(S), reverse_list(Output, Rev, []), print_list(Rev));
  (cross(S, N, Last, K, Out), not(member(N, Crossings)), solve(N, [N | Crossings], K, [Out | Output])).

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
%   setof([New, Node | Tree], (cross(Node, New, _, _, _), (member(New, [Node | Tree]), !)), NewProgress), !.
% 
% progress_tree(_, []).
% 
% done([State | _]) :-
%   end_state(State).

% Riddle start and end states
start_state([3, 3, 1, 0, 0]).
end_state([0, 0, 0, 3, 3]).

% Helper functions
printl(String) :- write(String), nl.
print_trip_right(S, N) :- write(S), write(' -> '), write(N), nl.
print_trip_left(S, N) :- write(N), write(' <- '), write(S), nl.
reverse_list([], Z, Z).
reverse_list([H | T], Z, Acc) :- reverse_list(T, Z, [H | Acc]).
print_list([]).
print_list([X | Y]) :- nl, write(X), print_list(Y).


% All possible river crossings rules, one hen crosses are removed as they will not occur...

% Left to right crossings
cross([HL, FL, 1, HR, FR], [HLN, FL, 0, HRN, FR], Last, Move, Out) :-
  not(Last == 0),
  Move is 0,
  HL >= 1,
  HLN is HL - 1,
  HRN is HR + 1,
  safe([HLN, FL, 0, HRN, FR]),
  Out = ['One hen crosses L -> R           ', HLN, FL, 0, HRN, FR].

cross([HL, FL, 1, HR, FR], [HLN, FL, 0, HRN, FR], Last, Move, Out) :-
  not(Last == 1),
  Move is 1,
  HL >= 2,
  HLN is HL - 2,
  HRN is HR + 2,
  safe([HLN, FL, 0, HRN, FR]),
  Out = ['Two hens cross L -> R            ', HLN, FL, 0, HRN, FR].

cross([HL, FL, 1, HR, FR], [HLN, FLN, 0, HRN, FRN], Last, Move, Out) :-
  not(Last == 2),
  Move is 2,
  HL >= 1,
  FL >= 1,
  HRN is HR + 1,
  FRN is FR + 1,
  HLN is HL - 1,
  FLN is FL - 1,
  safe([HLN, FLN, 0, HRN, FRN]),
  Out = ['One hen and one fox cross L -> R ', HLN, FLN, 0, HRN, FRN].

cross([HL, FL, 1, HR, FR], [HL, FLN, 0, HR, FRN], Last, Move, Out) :-
  not(Last == 3),
  Move is 3,
  FL >= 1,
  FLN is FL - 1,
  FRN is FR + 1,
  safe([HL, FLN, 0, HR, FRN]),
  Out = ['One fox cross L -> R             ', HL, FLN, 0, HR, FRN].

cross([HL, FL, 1, HR, FR], [HL, FLN, 0, HR, FRN], Last, Move, Out) :-
  not(Last == 4),
  Move is 4,
  FL >= 2,
  FLN is FL - 2,
  FRN is FR + 2,
  safe([HL, FLN, 0, HR, FRN]),
  Out = ['Two foxes cross L -> R           ', HL, FLN, 0, HR, FRN].

% Right to left crossings
cross([HL, FL, 0, HR, FR], [HLN, FL, 1, HRN, FR], Last, Move, Out) :-
  not(Last == 0),
  Move is 0,
  HR >= 1,
  HLN is HL + 1,
  HRN is HR - 1,
  safe([HLN, FL, 1, HRN, FR]),
  Out = ['One hen crosses L <- R           ', HLN, FL, 1, HRN, FR].

cross([HL, FL, 0, HR, FR], [HLN, FL, 1, HRN, FR], Last, Move, Out) :-
  not(Last == 1),
  Move is 1,
  HR >= 2,
  HLN is HL + 2,
  HRN is HR - 2,
  safe([HLN, FL, 1, HRN, FR]),
  Out = ['Two hens cross L <- R            ', HLN, FL, 1, HRN, FR].

cross([HL, FL, 0, HR, FR], [HLN, FLN, 1, HRN, FRN], Last, Move, Out) :-
  not(Last == 2),
  Move is 2,
  HR >= 1,
  FR >= 1,
  HRN is HR - 1,
  FRN is FR - 1,
  HLN is HL + 1,
  FLN is FL + 1,
  safe([HLN, FLN, 0, HRN, FRN]),
  Out = ['One hen and one fox cross L <- R ', HLN, FLN, 1, HRN, FRN].

cross([HL, FL, 0, HR, FR], [HL, FLN, 1, HR, FRN], Last, Move, Out) :-
  not(Last == 3),
  Move is 3,
  FR >= 1,
  FRN is FR - 1,
  FLN is FL + 1,
  safe([HL, FLN, 1, HR, FRN]),
  Out = ['One fox cross L <- R             ', HL, FLN, 1, HR, FRN].

cross([HL, FL, 0, HR, FR], [HL, FLN, 1, HR, FRN], Last, Move, Out) :-
  not(Last == 4),
  Move is 4,
  FR >= 2,
  FRN is FR - 2,
  FLN is FL + 2,
  safe([HL, FLN, 1, HR, FRN]),
  Out = ['Two foxes cross L <- R           ', HL, FLN, 1, HR, FRN].

% Safe function to make sure hens dont get eaten.
safe([HL, FL, _, HR, FR]) :- 
  ((HL >= FL; HL =:= 0), (HR >= FR; HR =:= 0)).
