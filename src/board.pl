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
            \+ member(Next, [Current | Rest])  % Ensure we dont revisit nodes
        ),
        NewPaths
    ),
    append(Queue, NewPaths, NewQueue),
    bfs(NewQueue, Board, Target, Path-Distance).

% Find all groups for a player
find_groups(Board, Player, Groups) :-
    findall(Group, player_group(Board, Player, Group), Groups).

player_group(Board, Player, Group) :-
    findall(Point, board_at(Board, Point, Player), Points),  % Collect all occupied points
    find_group(Board, Player, Points, [], Group).

% Find all stones connected to the current stone
find_group(Board, Player, [Point|Rest], Visited, Group) :-
    findall(AdjPoint,
            (
                adjacent_position(Point, AdjPoint),
                board_at(Board, AdjPoint, Player),
                \+ member(AdjPoint, Visited)
            ),
            AdjPoints),
    append(Rest, AdjPoints, NewQueue),
    find_group(Board, Player, NewQueue, [Point|Visited], Group).
find_group(_, _, [], Group, Group).

has_one_group(Board, Player) :-
    find_groups(Board, Player, Groups),
    length(Groups, 1).

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

free_path(Board, Group1, Group2) :-
    bfs([Group1], Board, Group2).

% Check if a position is free
is_free_path(Board, Point) :-
    board_at(Board, Point, +).  % Free positions are marked as '+'

has_free_path(Board, Player) :-
    find_groups(Board, Player, Groups),
    member(Group1, Groups),
    member(Group2, Groups),
    Group1 \= Group2,
    free_path(Board, Group1, Group2).
