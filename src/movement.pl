:- consult(board).
:- consult(utils).
:- use_module(library(random)).
:- use_module(library(system)).

% Execute a move in the game and update the game state.
%
% Arguments:
% - state(Board, CurrentPlayer, NextPlayer, OtherInfo): The current game state.
%   - Board: The 2D list representing the board.
%   - CurrentPlayer: The player making the move ('b' for Black, 'w' for White).
%   - NextPlayer: The player who will take the next turn.
%   - OtherInfo: Additional game-related metadata.
% - move(Source, Destination): The move to be performed.
%   - Source: The coordinates of the piece to move.
%   - Destination: The target coordinates where the piece will be moved.
% - NewGameState: The updated game state after the move is executed.
move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), move(Source, Destination), NewGameState) :-
    % 1. Get the element to move from the source position.
    board_at(Board, Source, NewElem),

    % 2. Replace the source position with an empty space ('+') to simulate the piece leaving.
    replace(Board, Source, +, TempBoard),

    % 3. Replace the destination position with the moved element (NewElem).
    replace(TempBoard, Destination, NewElem, NewBoard),

    % 4. Update the game state with the modified board and switch players.
    NewGameState = state(NewBoard, NextPlayer, CurrentPlayer, OtherInfo).

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
            \+removes_from_group(Board, Group, Source, Destination),
            board_at(Board, Source, CurrentPlayer),
            closer_to_friendly(Board, CurrentPlayer, Source, Destination)
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

% Finds all valid moves for the current player.
%
% Arguments:
% - state(Board, CurrentPlayer, _, _): The current game state, where:
%   - Board: The current board configuration.
%   - CurrentPlayer: The player whose turn it is to make a move.
%   - Other elements (not used in this part of the predicate, but can be used in other cases).
% - ListOfMoves: A list of valid moves for the current player, returned as output.
valid_moves(state(Board, CurrentPlayer, _, _), ListOfMoves) :-
    % 1. Find all the positions occupied by the current players pieces.
    find_player_positions(Board, CurrentPlayer, Positions),

    % 2. Convert these positions into the requested coordinate system (if necessary).
    maplist(convert_to_original_coords(Board), Positions, PositionsOG),

    % 3. Find all possible valid moves for the current players pieces.
    findall((Source, (DestinationX, DestinationY)),
        (
            member(Source, PositionsOG),
            adjacent_position(Source, (DestinationX, DestinationY)),     % For each position, find adjacent positions.
            valid_move(Board, CurrentPlayer, Source, (DestinationX, DestinationY)) % Check if the move is valid.
        ),
        SinglePieceMoves),

    % 4. Find all groups of the current players pieces.
    find_all_groups(Board, CurrentPlayer, Groups),

    % 5. For each group, find all valid moves for that group.
    findall(ValidMoves,
        (
            member(Group, Groups),
            valid_group_moves(Board, Group, ValidMoves)
        ),
        GroupMovesNested),

    % 6. Flatten the list of group moves (since `valid_group_moves` may return nested lists of moves).
    flatten(GroupMovesNested, GroupMoves),

    % 7. Combine single piece moves and group moves into one list of valid moves.
    append(SinglePieceMoves, GroupMoves, ListOfMoves).

human_move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), ListOfMoves, NewGameState) :-
    prompt_player_move(Source, Destination),
    attempt_move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), move(Source, Destination), ListOfMoves, NewGameState).

attempt_move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), move(Source, Destination), ListOfMoves, NewGameState) :-
    member((Source, Destination), ListOfMoves),
    move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), move(Source, Destination), NewGameState).

attempt_move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), _, ListOfMoves, NewGameState) :-
    write('Invalid move. Try again.'), nl,
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
