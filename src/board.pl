:- use_module(library(lists)).

shortest_path_with_distance(Board, Start, Target, Path, Distance) :-
    bfs([[Start]-0], Board, Target, RevPath-Distance),
    reverse(RevPath, Path).

bfs([[Target | Rest]-D | _], _, Target, [Target | Rest]-D):- % Found target
    !.
bfs([[Current | Rest]-D | Queue], Board, Target, Path-Distance) :-
    findall(
        [Next, Current | Rest]-(D + 1),
        (
            adjacent_position(Current, Next),
            (Next = Target; empty_position(Board, Next)),  % Allow target to be non-empty
            \+ member(Next, [Current | Rest])  % Ensure we don't revisit nodes
        ),
        NewPaths
    ),
    append(Queue, NewPaths, NewQueue),
    bfs(NewQueue, Board, Target, Path-Distance).

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

path((X, Y), (X1, Y1)) :- adjacent_position((X, Y), (X1, Y1)).
path((X, Y), (X2, Y2)) :- adjacent_position((X, Y), (X1, Y1)), path((X1, Y1), (X2, Y2)).

get_pos(Board, (X, Y), PlayerOrEmpty) :- % Transform to (1,1) at bottom left corner coordinates
    nth1(RowIndex, Board, Row),
    nth1(X, Row, PlayerOrEmpty).

board_at(Board, (X, Y), PlayerOrEmpty) :- % Transform to (1,1) at bottom left corner coordinates
    length(Board, TotalRows),
    RowIndex is TotalRows - Y + 1,
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
