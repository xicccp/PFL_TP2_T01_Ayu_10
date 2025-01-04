% This predicate converts the raw input (list of character codes) into a tuple of numbers
parse_coordinates(Input, (X, Y)) :-
    append(XCodes, [32|YCodes], Input),
    number_codes(X, XCodes),
    number_codes(Y, YCodes).

clear_screen :- write('\033[2J\033[H'). % ANSI escape sequence to clear the screen