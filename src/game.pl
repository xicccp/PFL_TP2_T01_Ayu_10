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
    -> initial_state(BoardSize, Player1, Player2, GameState),
       display_game(GameState),
       game_loop(GameState)
    ;  write('Invalid board size. Try again.'), nl,
       setup_game(Player1, Player2)
    ).

% Initializes the game state
initial_state(BoardSize, Player1, Player2, game_state(Board, Player1, Player2, black)) :-
    initialize_board(BoardSize, EmptyBoard),
    initialize_stones(EmptyBoard, Board).

% Creates an empty board
initialize_board(Size, Board) :-
    findall(Row, (length(Row, Size), maplist(=(empty), Row)), Board).

% Fills the board with initial stones
initialize_stones(EmptyBoard, Board) :-
    length(EmptyBoard, Size),
    maplist(initialize_row(Size), EmptyBoard, Board).

% Adds stones to a row based on their position
initialize_row(Size, Row, NewRow) :-
    length(Row, Size),
    findall(Stone, (nth1(X, Row, _), assign_stone(Size, X, Stone)), NewRow).

% Assigns the initial stone placement
assign_stone(Size, X, Stone) :-
    (  even(X), odd(Size) -> Stone = black
    ;  odd(X), even(Size) -> Stone = white
    ;  Stone = empty
    ).

% Display the game state
display_game(game_state(Board, Player1, Player2, CurrentPlayer)) :-
    nl, write('Current Player: '), write(CurrentPlayer), nl,
    write('  '), display_column_headers(Board), nl,
    display_rows(Board, 1),
    write('Player 1 (Black): '), write(Player1), nl,
    write('Player 2 (White): '), write(Player2), nl.

% Display column headers
display_column_headers(Board) :-
    length(Board, Size),
    findall(Col, between(1, Size, Col), Columns),
    maplist(format(' ~w'), Columns).

% Display rows with coordinates
display_rows([], _).
display_rows([Row|Rest], RowIndex) :-
    length(Row, Size),
    RowCoord is Size - RowIndex + 1,
    format('~|~`0t~d~2+ ', [RowCoord]), % Display row coordinate
    display_row(Row), nl,
    NextRowIndex is RowIndex + 1,
    display_rows(Rest, NextRowIndex).

% Display a single row
display_row([]).
display_row([Cell|Rest]) :-
    display_cell(Cell),
    display_row(Rest).

% Display a single cell
display_cell(black) :- write(' B ').
display_cell(white) :- write(' W ').
display_cell(empty) :- write(' . ').
