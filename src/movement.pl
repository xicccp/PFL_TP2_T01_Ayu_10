:- consult(board).
:- consult(utils).
:- use_module(library(random)).
:- use_module(library(system)).

move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), move(Source, Destination), NewGameState) :-
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

% Returns a list of ValidMoves for groups
valid_group_moves(Board, Group, ValidMoves) :-
    findall(
        ((AdjacentX, AdjacentY)),
        (   
            member(GroupPiece, Group),
            adjacent_position(GroupPiece, (AdjacentX, AdjacentY)),
            position_valid(Board, (AdjacentX, AdjacentY))
        ), Edges
    ),
    findall(
        (Source, Destination),
        (
            member(Source, Group),
            member(Destination, Edges),
            \+member(Destination, Group),
            \+removes_from_group(Board, Group, Source, Destination)
        ),
        ValidMoves
    ).

% finds out if a move will remove a piece from a group
removes_from_group(Board, Group, Source, Destination) :-
    select(Source, Group, NewGroup),
    \+ still_adjacent_to_group(Destination, NewGroup).

still_adjacent_to_group(Destination, Group) :-
    member(GroupPiece, Group),                    % For each piece in the group
    adjacent_position(Destination, GroupPiece),   % Check if the destination is adjacent
    !.                                            

% Finds all valid moves for the current player
valid_moves(state(Board, CurrentPlayer,_,_), ListOfMoves) :-
    find_player_positions(Board, CurrentPlayer, Positions), % Find all player positions
    maplist(convert_to_original_coords(Board), Positions, PositionsOG), % Convert to requested coordinate system
    findall((Source, (DestinationX, DestinationY)),
            (member(Source, PositionsOG),
            adjacent_position(Source, (DestinationX, DestinationY)),                         % For each stone in the positions
            valid_move(Board, CurrentPlayer, Source, (DestinationX, DestinationY))), % Check if move is valid
            SinglePieceMoves),
    find_all_groups(Board, CurrentPlayer, Groups),
    findall(ValidMoves,
    (
        member(Group, Groups),
        valid_group_moves(Board, Group, ValidMoves)
    ), GroupMovesNested),
    flatten(GroupMovesNested, GroupMoves),
    append(SinglePieceMoves, GroupMoves, ListOfMoves).

human_move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), ListOfMoves, NewGameState) :-
    prompt_player_move(Source, Destination),
    attempt_move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), move(Source, Destination), ListOfMoves, NewGameState).

attempt_move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), move(Source, Destination), ListOfMoves, NewGameState) :-
    member((Source, Destination), ListOfMoves),
    move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), move(Source, Destination), NewGameState).

attempt_move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), _, ListOfMoves, NewGameState) :-
    write('Invalid move. Try again.'), nl,
    write(ListOfMoves), nl,
    human_move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), ListOfMoves, NewGameState).

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

choose_move(ListOfMoves, 1, move(Source, Destination)) :-
    random_member((Source, Destination), ListOfMoves). % Pick a random move from the list
