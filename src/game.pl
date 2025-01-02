play :-
    write('Welcome to Ayu!'), nl,
    write('1. Play Human vs Human'), nl,
    write('2. Play Human vs Computer'), nl,
    write('3. Exit'), nl,
    write('Choose an option: '),
    read(Choice),
    handle_choice(Choice).

handle_choice(1) :- write('Human vs Human selected!'), nl.
handle_choice(2) :- write('Human vs Computer selected!'), nl.
handle_choice(3) :- write('Exiting game. Goodbye!'), nl.
handle_choice(_) :- write('Invalid choice!'), nl, play.
