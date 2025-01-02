:- consult(configuration).

% Main entry point
play :-
    write('--- Welcome to Ayu ---'), nl,
    write('1. Play Ayu'), nl,
    write('2. Exit'), nl,
    write('Choose an option: '),
    read(Choice),
    handle_menu_choice(Choice).

% Handle menu options
handle_menu_choice(1) :- % Start the game
    choose_game_type.
handle_menu_choice(2) :- % Exit the game
    write('Goodbye!'), nl.
handle_menu_choice(_) :- % Invalid option
    write('Invalid choice. Try again.'), nl,
    play.

% Choose game type
choose_game_type :-
    write('Choose game type:'), nl,
    write('1. Human vs Human (H/H)'), nl,
    write('2. Human vs Computer (H/PC)'), nl,
    write('3. Computer vs Human (PC/H)'), nl,
    write('4. Computer vs Computer (PC/PC)'), nl,
    write('Choose an option (1-4): '),
    read(TypeChoice),
    handle_game_type_choice(TypeChoice).

% Handle game type selection
handle_game_type_choice(1) :-
    setup_game(human, human).
handle_game_type_choice(2) :-
    setup_game(human, computer).
handle_game_type_choice(3) :-
    setup_game(computer, human).
handle_game_type_choice(4) :-
    setup_game(computer, computer).
handle_game_type_choice(_) :-
    write('Invalid choice. Try again.'), nl,
    choose_game_type.

% Game setup
setup_game(Player1, Player2) :-
    write('Enter board size (between 9 and 15): '),
    read(BoardSize),
    (  integer(BoardSize), BoardSize >= 9, BoardSize =< 15, BoardSize mod 2 =:= 1
    -> initial_state(config(BoardSize, Player1, Player2), GameState),
       display_game(GameState),
       game_loop(GameState)
    ;  write('Invalid board size. Try again.'), nl,
       setup_game(Player1, Player2)
    ).

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

game_loop(state(Board, CurrentPlayer, OtherInfo)) :-
    write('In progress...'), nl.