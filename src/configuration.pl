:- use_module(library(lists)).

% generate the initial board state for the game based on the configuration
initial_state(config(BoardSize, Player1Type, Player2Type), state(Board, CurrentPlayer, OtherInfo)) :-
    generate_board(BoardSize, Board), % generate the board configuration
    CurrentPlayer = Player1Type, % set player 1 as the initial player
    OtherInfo = other_info([Player1Type, Player2Type], [Player1Type, Player2Type], []).

% Update the game state by transitioning to the next player
update_game_state(state(Board, CurrentPlayer, OtherInfo), NewBoard, CurrentPlayer, state(NewBoard, NextPlayer, OtherInfo)) :-
    other_player(CurrentPlayer, NextPlayer).  % Find the other player for the next turn

% This predicate switches between Player1 and Player2
other_player(player1, player2).
other_player(player2, player1).

% generates a board with size NxN
generate_board(N, Board) :-
    length(Board, N),
    generate_rows(N, 1, Board).

% helper predicate to generate rows
generate_rows(N, Index, [Row|Rest]) :-
    Index =< N,
    generate_row(N, Index, Row),
    NextIndex is Index + 1,
    generate_rows(N, NextIndex, Rest).
generate_rows(N, Index, []) :-
    Index > N.

% generates a single row with alternating pieces and empty intersections
generate_row(N, Index, Row) :-
    (Index mod 2 =:= 0 -> alternating_row(N, Row, +, b); alternating_row(N, Row, w, +)).

% alternates between two symbols for N positions
alternating_row(0, [], _, _).
alternating_row(N, [First|Rest], FirstSymbol, SecondSymbol) :-
    N > 0,
    (N mod 2 =:= 0 -> First = SecondSymbol ; First = FirstSymbol),
    NextN is N - 1,
    alternating_row(NextN, Rest, FirstSymbol, SecondSymbol).