:- use_module(library(lists)).

% shortest_path_distance/4: Computes the shortest path distance between two points.
% Board: The game board.
% Source: The starting position.
% Target: The target position.
% Distance: The shortest distance (number of moves) between Source and Target.
shortest_path_distance(Board, Source, Target, Distance) :-
    bfs(Board, [Source], Target, [], Distance).

% bfs/5: Implements a breadth-first search to find the shortest path.
% Board: The game board.
% Frontier: The current set of positions being explored.
% Target: The position we're trying to reach.
% Visited: A list of already visited positions to prevent loops.
% Distance: The distance to the target.
bfs(_, [Target|_], Target, _, 0).
bfs(Board, [Current|Rest], Target, Visited, Distance) :-
    findall(Neighbor, (adjacent_position(Current, Neighbor), 
                       board_at(Board, Neighbor, +),
                       \+ member(Neighbor, Visited)),
                    Neighbors),
    append(Rest, Neighbors, NewFrontier),
    bfs(Board, NewFrontier, Target, [Current|Visited], SubDistance),
    Distance is SubDistance + 1.

/* find_group(Board, Player, Group) :-
    find_player_stones(Board, Player, Stones),
    explore_group(Board, Stones, Group).

find_player_groups(Board, Player, Groups) :-
    findall(Group, find_group(Board, Player, Group), Groups).

find_player_stones(Board, Player, Stones) :-
    findall((X, Y), (position_valid(Board, (X, Y)), board_at(Board, (X, Y), Player)), Stones).

explore_group(Board, Stones, Group) :-
    % This function explores all stones connected to the initial group.
    % A stone is connected if it is adjacent to any other stone in the group.
    % The exploration should respect orthogonal connectivity.
    explore_connected(Board, Stones, [], Group).

explore_connected(_, [], Visited, Visited).
explore_connected(Board, [Stone|Rest], Visited, Group) :-
    \+ member(Stone, Visited),
    find_adjacent(Board, Stone, Adjacent),
    append(Adjacent, Rest, NextStones),
    explore_connected(Board, NextStones, [Stone|Visited], Group). */

replace(Board, (X, Y), NewElem, NewBoard) :-
    length(Board, TotalRows),
    RowIndex is TotalRows - Y + 1, % Transform to (1,1) at bottom left corner coordinates
    nth1(RowIndex, Board, Row),
    replace_row(Row, X, NewElem, NewRow),
    replace_in_list(Board, RowIndex, NewRow, NewBoard).

% Replaces an element in a row
replace_row([_|Tail], 1, NewElem, [NewElem|Tail]).
replace_row([Head|Tail], Index, NewElem, [Head|NewTail]) :-
    Index > 1,
    Index1 is Index - 1,
    replace_row(Tail, Index1, NewElem, NewTail).

% Replaces a row in the board
replace_in_list([_|Tail], 1, NewRow, [NewRow|Tail]).
replace_in_list([Head|Tail], Index, NewRow, [Head|NewTail]) :-
    Index > 1,
    Index1 is Index - 1,
    replace_in_list(Tail, Index1, NewRow, NewTail).

% Checks if the position is adjacent
adjacent_position((X, Y), (X1, Y)) :-
    X1 is X + 1.
adjacent_position((X, Y), (X1, Y)) :-
    X1 is X - 1.
adjacent_position((X, Y), (X, Y1)) :-
    Y1 is Y + 1.
adjacent_position((X, Y), (X, Y1)) :-
    Y1 is Y - 1.

board_at(Board, (X, Y), PlayerOrEmpty) :-
    length(Board, TotalRows),
    RowIndex is TotalRows - Y + 1, % Transform to (1,1) at bottom left corner coordinates
    nth1(RowIndex, Board, Row),
    nth1(X, Row, PlayerOrEmpty).

empty_position(Board, (X, Y)) :-
    board_at(Board, (X, Y), +).  % Check if the position is empty

position_valid(Board, (X, Y)) :-
    length(Board, Rows),        % Get the number of rows
    nth1(1, Board, FirstRow),   % Get the first row to determine the number of columns
    length(FirstRow, Cols),
    X >= 1, X =< Cols,           % Check if X is within the column range
    Y >= 1, Y =< Rows.           % Check if Y is within the row range
