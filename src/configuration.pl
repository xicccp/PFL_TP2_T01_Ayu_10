:- use_module(library(lists)).

% generate the initial board state for the game based on the configuration
initial_state(config(BoardSize, Player1Type, Player2Type), state(Board, CurrentPlayer, OtherInfo)) :-
    generate_board(BoardSize, Board), % generate the board configuration
    CurrentPlayer = Player1Type, % set player 1 as the initial player
    OtherInfo = other_info([Player1Type, Player2Type], [Player1Type, Player2Type], []).
  
% generates a board with size NxN
generate_board(N, Board) :-
    length(Board, N),
    findall(Row, (generate_index(1, N, Index), generate_row(N, Index, Row)), Board).

% generates a single row with alternating black and white stones
generate_row(N, Index, Row) :- 
    length(Row, N),
    (Index mod 2 =:= 0 -> alternating_row(Row, w); alternating_row(Row, b)).

% alternates between black and white stones
alternating_row([], _).
alternating_row([Stone|Rest], b) :-
    Stone = b,
    alternating_row(Rest, w).    
alternating_row([Stone|Rest], w) :-
    Stone = w,
    alternating_row(Rest, b).

% generates indices from Start to End
generate_index(Start, End, Start) :-
    Start =< End.
generate_index(Start, End, Index) :-
    Start < End,
    Next is Start + 1,
    generate_index(Next, End, Index).