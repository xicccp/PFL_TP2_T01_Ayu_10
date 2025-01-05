% Display the entire game state
display_game(state(Board, CurrentPlayer, NextPlayer, OtherInfo)) :-
    write('\33\[2J'), % Clear the screen
    OtherInfo = other_info(PlayerTypes, PlayerNames, _),
    nth1(1, PlayerNames, Player1Name),
    nth1(2, PlayerNames, Player2Name),
    nth1(1, PlayerTypes, Player1Type),
    nth1(2, PlayerTypes, Player2Type),
    nl, write('Current player: '), write(CurrentPlayer), nl, nl,

    % Get the board size and display the board with column headers
    length(Board, BoardSize),  % Get the size of the board
    write('   '), display_column_headers(BoardSize), nl,  % Pass the board size for column headers
    display_rows(Board, 1, BoardSize), nl,  % Pass BoardSize to calculate the row numbering

    % Display player information
    write('Player 1 (black/b): '), write(Player1Type), nl,
    write('Player 2 (white/w): '), write(Player2Type), nl.

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
