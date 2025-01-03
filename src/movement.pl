:- consult(board).

move(GameState, Move, NewGameState) :-
    GameState = state(Board, CurrentPlayer, OtherInfo),
    Move = (Source, Destination), % Unify Move with (Source, Destination)
    valid_move(Board, CurrentPlayer, Source, Destination), % Validate move
    execute_move(Board, Source, Destination, NewBoard),    % Execute move
    update_game_state(state(Board, CurrentPlayer, OtherInfo),
                      NewBoard, CurrentPlayer, NewGameState).

valid_moves(GameState, ListOfMoves) :-
    GameState = state(Board, CurrentPlayer, _),
    find_player_groups(Board, CurrentPlayer, Groups),   % Identify player groups of stones
    findall((Source, Destination),
            (member(Group, Groups),
             member(Source, Group),                         % For each stone in the group
             adjacent_empty_point(Board, Source, Destination), % Find adjacent empty points
             valid_move(Board, CurrentPlayer, Source, Destination)), % Check if move is valid
            ListOfMoves).

execute_move(Board, Source, Destination, NewBoard) :-
    replace(Board, Source, empty, TempBoard),               % Remove stone from Source
    replace(TempBoard, Destination, stone, NewBoard).       % Place stone at Destination

human_move(Board, NewBoard, NextPlayer) :-
    write('Enter your move (Source, Destination): '),
    read(Move), % Read the move as a single variable
    Move = (Source, Destination), % Ensure the move is a tuple
    move(state(Board, human, _), Move, state(NewBoard, NextPlayer, _)).

computer_move(Board, NewBoard, NextPlayer) :-
    valid_moves(state(Board, computer, _), ListOfMoves),
    random_member(Move, ListOfMoves), % Pick a random move
    move(state(Board, computer, _), Move, state(NewBoard, NextPlayer, _)).
