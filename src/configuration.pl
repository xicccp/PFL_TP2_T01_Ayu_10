% generate the initial board state for the game based on the configuration
initial_state(+GameConfig(BoardSize, Player1Type, Player2Type), -GameState(Board, CurrentPlayer, OtherInfo)) :-
    generate_board(BoardSize, Board), % generate the board configuration
    CurrentPlayer = Player1Type, % set player 1 as the initial player
    OtherInfo = other_info{
        player_types: [Player1Type, Player2Type],
        player_names: [Player1Type, Player2Type], % set the names to their type for now
        move_history: []
    }.
  
% generates a board with size NxN
generate_board(N, Board) :-
    findall(Row, generate_row(N, Row), Board).

% gemerates a single row with alternating black and white stones
generate_row(N, Row) :- 
    length(Row, N),
    alternating_row(Row, b). % start with black

% alternates between black and white stones
alternating_row([], _).
alternating_row([Stone|Rest], b) :-
    Stone = b,
    alternating_row(Rest, w).    
alternating_row([Stone|Rest], w) :-
    Stone = w,
    alternating_row(Rest, b).