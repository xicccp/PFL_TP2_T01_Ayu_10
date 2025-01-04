:- consult(configuration).
:- consult(display).
:- consult(movement).

% Main entry point
play :-
    write('--- Welcome to Ayu ---'), nl,
    write('1. Play Ayu'), nl,
    write('2. Exit'), nl,
    write('Choose an option: '),
    read(Choice),
    write('\33\[2J'), % Clear the screen
    handle_menu_choice(Choice).

% Handle menu options
handle_menu_choice(1) :- % Start the game
    choose_game_type.
handle_menu_choice(2) :- % Exit the game
    write('\33\[2J'), % Clear the screen
    write('Goodbye!'), nl.
handle_menu_choice(_) :- % Invalid option
    write('\33\[2J'), % Clear the screen
    write('Invalid choice. Try again.'), nl,
    play.

% Choose game type
choose_game_type :-
    write('\33\[2J'), % Clear the screen
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
    write('\33\[2J'), % Clear the screen
    write('Invalid choice. Try again.'), nl,
    choose_game_type.

% Game setup
setup_game(Player1, Player2) :-
    write('\33\[2J'), % Clear the screen
    write('Enter board size (odd number between 9 and 15): '),
    read(BoardSize),
    validate_board_size(BoardSize, Player1, Player2).

% Helper predicate to validate the board size
validate_board_size(BoardSize, Player1, Player2) :-
    integer(BoardSize),
    BoardSize >= 9,
    BoardSize =< 15,
    BoardSize mod 2 =:= 1,
    initial_state(config(BoardSize, Player1, Player2), GameState),
    game_loop(GameState).

validate_board_size(_, Player1, Player2) :-
    write('\33\[2J'), % Clear the screen
    write('Invalid board size. Try again.'), nl,
    setup_game(Player1, Player2).

% Main game loop
game_loop(GameState) :-
    write('\33\[2J'), % Clear the screen
    % Display the updated game state
    display_game(GameState),
    
    % Handle the move for the current player (generic handling of player move)
    current_player_move(GameState, NewGameState),
    game_loop(NewGameState).


% Handling the current player's move (either human or computer)
current_player_move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), NewGameState) :-
    OtherInfo = other_info(PlayerTypes, _, _),
    nth1(1, PlayerTypes, Player1Type),
    nth1(2, PlayerTypes, Player2Type),
    (CurrentPlayer = b -> PlayerType = Player1Type ; PlayerType = Player2Type),
    handle_player_move(PlayerType, state(Board, CurrentPlayer, NextPlayer, OtherInfo), NewGameState).

% Handling the current players move (either human or computer)
handle_player_move(human, state(Board, CurrentPlayer, NextPlayer, OtherInfo), NewGameState) :-
    human_move(state(Board, CurrentPlayer, NextPlayer, OtherInfo), NewGameState),
    !.

handle_player_move(computer, state(Board, CurrentPlayer, NextPlayer, OtherInfo), NewGameState) :-
    computer_move(Board, NewBoard, NextPlayer),
    !.

% End the game if a condition is met
game_over(state(_, _, _, _)) :-
    % Implement game-over logic here (e.g., victory condition)
    fail.