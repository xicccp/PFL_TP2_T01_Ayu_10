:- consult('../src/board.pl').
:- consult('../src/movement.pl').
% A simple 5x5 board for testing:
% (0,0) is the bottom-left, (4,4) is the top-right
test_board([
    [+,+,+,+,+],
    [+,+,w,+,+],
    [+,+,+,b,+],
    [w,w,+,+,+],
    [b,b,+,+,+]
]).

test_board9([
    [+,+,+,+,+,+,+,+,+],
    [+,+,+,+,+,+,+,+,+],
    [+,+,+,+,+,+,+,+,+],
    [+,+,+,b,b,b,+,+,+],
    [+,+,+,b,+,b,+,+,+],
    [+,+,+,b,b,b,+,+,+],
    [w,+,+,+,+,+,+,+,+],
    [w,+,+,+,+,+,+,+,+],
    [+,+,w,+,w,+,+,+,+]
]).

tester([
    [b, +, b, +, b, +, b, +, b],
    [+, b, +, b, +, b, +, b, +],
    [b, +, b, +, b, +, b, +, b],
    [+, b, +, b, +, b, +, b, +],
    [b, +, b, +, b, +, b, +, b],
    [+, b, +, b, +, b, +, b, +],
    [b, +, b, +, b, +, b, +, b],
    [+, b, +, b, +, b, +, b, +],
    [b, +, b, +, b, +, b, +, b]
]).

% Test board_at/3
test(board_at) :-
    test_board(Board),
    board_at(Board, (3, 3), +), 
    board_at(Board, (3, 4), w),  
    board_at(Board, (1, 1), b).  

% Test empty_position/2
test(empty_position) :-
    test_board(Board),
    empty_position(Board, (3, 3)),  % Empty position
    \+ empty_position(Board, (3, 4)).  % Occupied by white piece

% Test position_valid/2
test(position_valid) :-
    test_board(Board),
    position_valid(Board, (1, 1)),  % Bottom-left corner
    position_valid(Board, (4, 4)),  % Top-right corner
    \+position_valid(Board, (0, 0)),  % Out of bounds
    position_valid(Board, (5, 5)).   % Out of bounds

% Test adjacent_position/2
test(adjacent_position) :-
    adjacent_position((2, 2), (3, 2)),  % Right
    adjacent_position((2, 2), (1, 2)),  % Left
    adjacent_position((2, 2), (2, 3)),  % Up
    adjacent_position((2, 2), (2, 1)),  % Down
    \+adjacent_position((2, 2), (4, 1)).  % False

% Test replace/4
test(replace) :-
    test_board(Board),
    replace(Board, (3, 3), +, NewBoard),  % Replace black piece with empty
    board_at(NewBoard, (3, 3), +),
    replace(NewBoard, (3, 3), b, RestoredBoard),  % Restore black piece
    board_at(RestoredBoard, (3, 3), b).

% Test shortest_path_distance/4
test(shortest_path_distance) :-
    test_board(Board),
    % shortest_path_distance(Board, (1, 1), (3, 4), 5),  % Shortest path to white
    shortest_path_distance(Board, (1, 1), (1, 3), 1),  % Shortest path to black
    shortest_path_distance(Board, (1, 1), (1, 1), 0).  % Same position
