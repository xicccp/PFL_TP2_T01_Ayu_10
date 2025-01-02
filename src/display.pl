% Display the game state
display_game(state(Board, CurrentPlayer, OtherInfo)) :-
    OtherInfo = other_info(_, PlayerNames, _), % Extract player names
    nth1(1, PlayerNames, Player1Name),
    nth1(2, PlayerNames, Player2Name),
    nl, write('Current player: '), write(CurrentPlayer), nl,

    % Display the board and column headers
    write('  '), display_column_headers(Board), nl,
    display_rows(Board, 1), nl,

    % Display player names
    write('Player 1 (black): '), write(Player1Name), nl,
    write('Player 2 (white): '), write(Player2Name), nl.

% Display column headers
display_column_headers(Board) :-
    length(Board, Size),
    write('   '), % Offset for row numbers
    display_numbers(1, Size), !.

% Display numbers from Start to End.
display_numbers(Start, End) :-
    Start =< End,
    format('~|~t~d~2+', [Start]), % Align numbers to 2 spaces
    Next is Start + 1,
    display_numbers(Next, End).
display_numbers(_, _). % Stop when Start > End.

% Display all rows with row numbers.
display_rows([], _). % Base case: no more rows.
display_rows([Row|Rest], RowNumber) :-
    format('~|~t~d~2+ ', [RowNumber]), % Align row numbers
    display_row(Row),
    nl,
    NextRow is RowNumber + 1,
    display_rows(Rest, NextRow).

% Display a single row.
display_row([]). % Base case: no more elements in the row.
display_row([Cell|Rest]) :-
    display_cell(Cell),
    display_row(Rest).

% Display a single cell.
display_cell(b) :- write(' b '). % Black piece
display_cell(w) :- write(' w '). % White piece
display_cell(empty) :- write(' . '). % Empty cell