% Display the entire game state.
%
% Arguments:
% - state(Board, CurrentPlayer, NextPlayer, OtherInfo): The game state to display.
%   - Board: The game board, represented as a 2D list of rows.
%   - CurrentPlayer: The player whose turn it currently is ('b' for Black, 'w' for White).
%   - NextPlayer: The player who will play next.
%   - OtherInfo: Metadata about the game, including player types and names.
display_game(state(Board, CurrentPlayer, NextPlayer, OtherInfo)) :-
    % Clear the screen to refresh the display for a clean view.
    write('\33\[2J'),

    % Extract player-related information from OtherInfo.
    OtherInfo = other_info(PlayerTypes, PlayerNames, _), % Unpack player types and names.
    nth1(1, PlayerNames, Player1Name),  % Get Player 1s name.
    nth1(2, PlayerNames, Player2Name),  % Get Player 2s name.
    nth1(1, PlayerTypes, Player1Type),  % Get Player 1s type (e.g., 'human' or 'AI').
    nth1(2, PlayerTypes, Player2Type),  % Get Player 2s type (e.g., 'human' or 'AI').

    % Display the current player and a blank line for spacing.
    nl, write('Current player: '), write(CurrentPlayer), nl, nl,

    % Display the board.
    % 1. Get the size of the board by calculating its length.
    length(Board, BoardSize),

    % 2. Display the column headers using BoardSize.
    write('   '), display_column_headers(BoardSize), nl,

    % 3. Display each row of the board, including row numbering.
    display_rows(Board, 1, BoardSize), nl,

    % Display additional player information.
    write('Player 1 (black/b): '), write(Player1Type), nl,  % Show Player 1s type.
    write('Player 2 (white/w): '), write(Player2Type), nl.  % Show Player 2s type.

% Display column headers
display_column_headers(BoardSize) :-
    display_numbers(1, BoardSize).  % Adjusted to display correct number of columns

% Display numbers for column headers
display_numbers(Start, End) :-
    Start =< End,
    format('~|~t~d~3+', [Start]), % Align column numbers
    Next is Start + 1,
    display_numbers(Next, End).
display_numbers(_, _). % Stop when Start > End.

% Display all rows with row numbers (adjusted for bottom-left coordinates)
display_rows([], _, _). % No rows to display
display_rows([Row|Rest], RowNumber, TotalRows) :-
    % Calculate the new row number to be printed (1 = bottom row)
    DisplayRowNumber is TotalRows - RowNumber + 1,
    format('~|~t~d~3| ', [DisplayRowNumber]), % Align row numbers
    display_row(Row), nl,
    NextRow is RowNumber + 1,
    display_rows(Rest, NextRow, TotalRows).

% Display a single row
display_row([]).
display_row([Cell|Rest]) :-
    display_cell(Cell),
    display_row(Rest).

% Display a single cell with custom symbols
display_cell(b) :- write(' b '). % Black piece
display_cell(w) :- write(' w '). % White piece
display_cell(+) :- write(' + '). % Empty intersection
