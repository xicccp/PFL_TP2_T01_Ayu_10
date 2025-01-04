:- consult(board).
:- consult(utils).

move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), move(Source, Destination), NewGameState) :-
    valid_move(Board, CurrentPlayer, Source, Destination), % Validate move
    board_at(Board, Source, NewElem),                      % Get the element to move
    replace(Board, Source, +, TempBoard),    % Execute move
    replace(TempBoard, Destination, NewElem, NewBoard),
    NewGameState = state(NewBoard, NextPlayer, CurrentPlayer, OtherInfo).  % Update game state

closer_to_friendly(Board, CurrentPlayer, Source, Destination) :-
    findall(Pos, board_at(Board, Pos, CurrentPlayer), FriendlyPositions), % Find all friendly positions
    findall(Dist-Friend, member(Friend, FriendlyPositions), shortest_path_distance(Board, Source, Friend, Dist), Distances), % Calculate distances to friendly positions
    min_dist(ClosestDistance-ClosestFriend, Distances), % Find the closest friendly position
    shortest_path_distance(Board, Destination, ClosestFriend, NewDistance),
    NewDistance < ClosestDistance. % Check if the new distance is less than the closest distance

% Check if a move is valid
valid_move(Board, CurrentPlayer, Source, Destination) :-
    % 1. Check if the source is occupied by a player piece
    board_at(Board, Source, CurrentPlayer),
    % 2. Check if the destination is empty
    board_at(Board, Destination, +),
    % 3. Check if the destination is orthogonally adjacent to the source
    adjacent_position(Source, Destination),
    % 4. Check if the destination brings the piece closer to closest friendly piece(s)
    closer_to_friendly(Board, CurrentPlayer, Source, Destination).

valid_moves(GameState, ListOfMoves) :-
    GameState = state(Board, CurrentPlayer, _),
    find_player_groups(Board, CurrentPlayer, Groups),   % Identify player groups of stones
    findall((Source, Destination),
            (member(Group, Groups),
             member(Source, Group),                         % For each stone in the group
             adjacent_empty_point(Board, Source, Destination), % Find adjacent empty points
             valid_move(Board, CurrentPlayer, Source, Destination)), % Check if move is valid
            ListOfMoves).

human_move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), NewGameState) :-
    write('Enter the source position (format "x,y."): '),
    read((SourceX, SourceY)),
    Source = (SourceX, SourceY),

    write('Enter the destination position (format "x,y."): '),
    read((DestinationX, DestinationY)),
    Destination = (DestinationX, DestinationY),

    move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), move(Source, Destination), NewGameState).

computer_move(Board, NewBoard, NextPlayer) :-
    valid_moves(state(Board, computer, _), ListOfMoves),
    random_member(Move, ListOfMoves), % Pick a random move
    move(state(Board, computer, _), Move, state(NewBoard, NextPlayer, _)).
