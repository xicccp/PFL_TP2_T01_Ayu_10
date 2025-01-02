% Display the entire game state
display_game(state(Board, CurrentPlayer, OtherInfo)) :-
    OtherInfo = other_info(_, PlayerNames, _),
    nth1(1, PlayerNames, Player1Name),
    nth1(2, PlayerNames, Player2Name),
    nl, write('Current player: '), write(CurrentPlayer), nl, nl,

    % Display the board with column headers
    write('     '), display_column_headers(Board), nl,
    display_rows(Board, 1), nl,

    % Display player information
    write('Player 1 (black): '), write(Player1Name), nl,
    write('Player 2 (white): '), write(Player2Name), nl.

% Display column headers
display_column_headers(Board) :-
    length(Board, Size),
    display_numbers(1, Size).

% Display numbers for column headers
display_numbers(Start, End) :-
    Start =< End,
    format('~|~t~d~3+', [Start]), % Align column numbers
    Next is Start + 1,
    display_numbers(Next, End).
display_numbers(_, _). % Stop when Start > End.

% Display all rows with row numbers
display_rows([], _).
display_rows([Row|Rest], RowNumber) :-
    format('~|~t~d~3| ', [RowNumber]), % Align row numbers
    display_row(Row), nl,
    NextRow is RowNumber + 1,
    display_rows(Rest, NextRow).

% Display a single row
display_row([]).
display_row([Cell|Rest]) :-
    display_cell(Cell),
    display_row(Rest).

% Display a single cell with custom symbols
display_cell(b) :- write(' b '). % Black piece
display_cell(w) :- write(' w '). % White piece
display_cell(+) :- write(' + '). % Empty intersection
