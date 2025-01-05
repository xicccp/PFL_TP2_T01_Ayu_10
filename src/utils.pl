convert_to_original_coords(Board, (X, Y1), (X, Y)) :-
    length(Board, TotalRows),
    Y is TotalRows - Y1 + 1.  % Transform Y to match requested

subset([], _).
subset([H|T], Set) :-
    member(H, Set),
    subset(T, Set).

% Base case: an empty list is already flat
flatten([], []).
flatten([Head|Tail], FlatList) :-
    is_list(Head),                          
    flatten(Head, FlatHead),                 
    flatten(Tail, FlatTail),                 
    append(FlatHead, FlatTail, FlatList).
flatten([Head|Tail], [Head|FlatTail]) :-
    \+ is_list(Head),               
    flatten(Tail, FlatTail).                 