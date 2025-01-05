:- consult(board).
:- consult(utils).

move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), move(Source, Destination), NewGameState) :-
    valid_move(Board, CurrentPlayer, Source, Destination), % Validate move
    board_at(Board, Source, NewElem),                      % Get the element to move
    replace(Board, Source, +, TempBoard),    % Execute move
    replace(TempBoard, Destination, NewElem, NewBoard),
    NewGameState = state(NewBoard, NextPlayer, CurrentPlayer, OtherInfo).  % Update game state

remove_source(Source, List, Result) :-
    select(Source, List, Result).

closer_to_friendly(Board, CurrentPlayer, Source, Destination) :-
    findall((X, Y), (nth1(Y, Board, Row), nth1(X, Row, CurrentPlayer)), FriendlyPositions), % Find all friendly positions
    maplist(convert_to_original_coords(Board), FriendlyPositions, FriendlyPositionsWithSource), 
    % Remove the source position from the FriendlyPositionsOG list
    remove_source(Source, FriendlyPositionsWithSource, FriendlyPositionsOG),
    findall(Dist-Posi, (
        member(Posi, FriendlyPositionsOG),
        shortest_path_with_distance(Board, Source, Posi, _, Distance),
        Dist is Distance  % Calculate distance from Source to each friendly position
    ), Distances),
    % Find the minimum distance to any friendly position from Source
    min_member(ClosestDist-_, Distances),
    findall(NewDist-NewPosi, (
        member(Posi, FriendlyPositionsOG),
        shortest_path_with_distance(Board, Destination, Posi, _, NewDistance),  % Calculate distance from Destination to each friendly position
        NewDist is NewDistance  % Calculate distance from Source to each friendly position
    ), NewDistances),
    % Find the minimum distance to any friendly position from Destination
    min_member(NewClosestDist-_, NewDistances),
    NewClosestDist < ClosestDist.

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
    find_player_positions(Board, CurrentPlayer, Positions), % Find all player positions
    findall((Source, Destination),
            (member(Source, Positions),                         % For each stone in the positions
             adjacent_empty_point(Board, Source, Destination), % Find adjacent empty points
             valid_move(Board, CurrentPlayer, Source, Destination)), % Check if move is valid
            ListOfMoves).

find_player_positions(Board, Player, Positions) :-
    findall((X, Y), (nth1(Y, Board, Row), nth1(X, Row, Player)), Positions).

human_move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), NewGameState) :-
    prompt_player_move(Source, Destination),
    attempt_move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), move(Source, Destination), NewGameState).

attempt_move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), move(Source, Destination), NewGameState) :-
    move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), move(Source, Destination), NewGameState),
    !.

attempt_move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), move(Source, Destination), NewGameState) :-
    write('Invalid move. Try again.'), nl,
    human_move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), NewGameState).

prompt_player_move(Source, Destination) :-
    get_source_position(Source),
    get_destination_position(Destination).

get_source_position(Source) :-
    write('Enter the source position (format "x,y."): '),
    read((SourceX, SourceY)),
    Source = (SourceX, SourceY).

get_destination_position(Destination) :-
    write('Enter the destination position (format "x,y."): '),
    read((DestinationX, DestinationY)),
    Destination = (DestinationX, DestinationY).

computer_move(Board, NewBoard, NextPlayer) :-
    valid_moves(state(Board, computer, _), ListOfMoves),
    random_permutation(ListOfMoves, [Move|_]), % Shuffle the list and pick the first move
    move(state(Board, computer, _), Move, state(NewBoard, NextPlayer, _)).
