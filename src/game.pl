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
    validate_board_size(BoardSize, Player1, Player2).

% Helper predicate to validate the board size
validate_board_size(BoardSize, Player1, Player2) :-
    integer(BoardSize),
    BoardSize >= 9,
    BoardSize =< 15,
    BoardSize mod 2 =:= 1,
    initial_state(config(BoardSize, Player1, Player2), GameState),
    display_game(GameState),
    game_loop(GameState).

validate_board_size(_, Player1, Player2) :-
    write('Invalid board size. Try again.'), nl,
    setup_game(Player1, Player2).

% Main game loop
game_loop(state(Board, CurrentPlayer, OtherInfo), Player1Type, Player2Type) :-
    % Determine the next player type
    (CurrentPlayer = Player1Type, NextPlayer = Player2Type;
     CurrentPlayer = Player2Type, NextPlayer = Player1Type),
    
    % Handle the move for the current player (generic handling of player move)
    current_player_move(Board, NewBoard, CurrentPlayer, NextPlayer, state(NewBoard, NextPlayer, OtherInfo)),
    
    % Update the game state (after move)
    update_game_state(state(Board, CurrentPlayer, OtherInfo), NewBoard, CurrentPlayer, state(NewBoard, NextPlayer, OtherInfo)),
    
    % Display the updated game state
    display_game(state(NewBoard, NextPlayer, OtherInfo)),
    
    % Recursively continue the game loop with the updated state
    game_loop(state(NewBoard, NextPlayer, OtherInfo), Player1Type, Player2Type).

% Handling the current players move (either human or computer)
current_player_move(Board, NewBoard, human, NextPlayer, NewGameState) :-
    human_move(Board, NewBoard, NextPlayer),
    !.

current_player_move(Board, NewBoard, computer, NextPlayer, NewGameState) :-
    computer_move(Board, NewBoard, NextPlayer),
    !.
