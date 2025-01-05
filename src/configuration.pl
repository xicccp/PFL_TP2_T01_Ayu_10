:- use_module(library(lists)).

% Generate the initial board state for the game based on the given configuration.
% 
% Arguments:
% - config(BoardSize, Player1Type, Player2Type): Specifies the size of the board and the types of the players.
% - state(Board, CurrentPlayer, NextPlayer, OtherInfo): The resulting initial state of the game, which includes:
%   - Board: The initial configuration of the board with pieces placed for both players.
%   - CurrentPlayer: The player who will take the first turn (Black).
%   - NextPlayer: The player who will take the next turn (White).
%   - OtherInfo: Additional information about the players and game context.

initial_state(config(BoardSize, Player1Type, Player2Type), state(Board, CurrentPlayer, NextPlayer, OtherInfo)) :-
    % 1. Generate the board with the specified size and initial placement of stones.
    generate_board(BoardSize, Board),
    
    % 2. Set the initial player to Black ('b').
    CurrentPlayer = b,

    % 3. Set the next player to White ('w').
    NextPlayer = w,

    % 4. Include additional information:
    %    - Player types (e.g., human or AI) provided in the configuration.
    %    - Player colors ('black' for Black, 'white' for White).
    %    - An empty list for any other game-specific metadata that might be added later.
    OtherInfo = other_info([Player1Type, Player2Type], [black, white], []).

losing_state(config(BoardSize, Player1Type, Player2Type), state(Board, CurrentPlayer, NextPlayer, OtherInfo)) :-
    BoardSize = 5,
    Board = [
        [+,b,b,+,+],
        [w,w,w,+,w],
        [+,+,b,+,+],
        [+,+,b,w,w],
        [+,+,b,b,+]
    ],
    CurrentPlayer = w,
    NextPlayer = b,
    OtherInfo = other_info([Player1Type, Player2Type], [black, white], []).

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
    (Index mod 2 =:= 0 -> alternating_row(N, Row, w, +); alternating_row(N, Row, +, b)).

% alternates between two symbols for N positions
alternating_row(0, [], _, _).
alternating_row(N, [First|Rest], FirstSymbol, SecondSymbol) :-
    N > 0,
    (N mod 2 =:= 0 -> First = SecondSymbol ; First = FirstSymbol),
    NextN is N - 1,
    alternating_row(NextN, Rest, FirstSymbol, SecondSymbol).