convert_to_original_coords(Board, (X, Y1), (X, Y)) :-
    length(Board, TotalRows),
    Y is TotalRows - Y1 + 1.  % Transform Y to match requested