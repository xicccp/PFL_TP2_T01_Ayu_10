:- use_module(library(lists)).
:- consult('../src/configuration').

% Test for initial_state/2
test_initial_state :-
    initial_state(config(3, player1, player2), state(Board, CurrentPlayer, OtherInfo)),
    Board == [[b, w, b], [w, b, w], [b, w, b]],
    CurrentPlayer == player1,
    OtherInfo == other_info([player1, player2], [player1, player2], []).

% Test for generate_board/2
test_generate_board :-
    generate_board(3, Board),
    Board == [[b, w, b], [w, b, w], [b, w, b]].

% Test for generate_row/2
test_generate_row :-
    generate_row(3, Row),
    Row == [b, w, b].

% Run all tests
run_tests :-
    test_initial_state,
    test_generate_board,
    test_generate_row,
    write('All tests passed.'), nl.

:- initialization(run_tests).