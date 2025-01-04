:- consult(board).

% This predicate switches between Player1 and Player2
other_player(player1, player2).
other_player(player2, player1).

move(state(Board, CurrentPlayer, OtherInfo), move(Source, Destination), NewGameState) :-
    valid_move(Board, CurrentPlayer, Source, Destination), % Validate move
    board_at(Board, Source, NewElem),                      % Get the element to move
    replace(Board, Source, +, TempBoard),    % Execute move
    replace(TempBoard, Destination, NewElem, NewBoard),
    other_player(CurrentPlayer, NextPlayer),               % Switch player
    NewGameState = state(NewBoard, NextPlayer, OtherInfo).  % Update game state

valid_moves(GameState, ListOfMoves) :-
    GameState = state(Board, CurrentPlayer, _),
    find_player_groups(Board, CurrentPlayer, Groups),   % Identify player groups of stones
    findall((Source, Destination),
            (member(Group, Groups),
             member(Source, Group),                         % For each stone in the group
             adjacent_empty_point(Board, Source, Destination), % Find adjacent empty points
             valid_move(Board, CurrentPlayer, Source, Destination)), % Check if move is valid
            ListOfMoves).

human_move(Board, NewBoard, NextPlayer) :-
    write('Enter your move (Source, Destination): '),
    read(Move), % Read the move as a single variable
    Move = (Source, Destination), % Ensure the move is a tuple
    move(state(Board, human, _), Move, state(NewBoard, NextPlayer, _)).

computer_move(Board, NewBoard, NextPlayer) :-
    valid_moves(state(Board, computer, _), ListOfMoves),
    random_member(Move, ListOfMoves), % Pick a random move
    move(state(Board, computer, _), Move, state(NewBoard, NextPlayer, _)).
