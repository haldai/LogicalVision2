%% background knowledge about positions

left_next_to(L1, L2):-
    next_to(L1, L2),
    sp_location(L1, [X1, _]),
    sp_location(L2, [X2, _]),
    X1 < X2.
right_next_to(L1, L2):-
    next_to(L1, L2),
    sp_location(L1, [X1, _]),
    sp_location(L2, [X2, _]),
    X1 > X2.
up_next_to(L1, L2):-
    next_to(L1, L2),
    sp_location(L1, [_, Y1]),
    sp_location(L2, [_, Y2]),
    Y1 < Y2.
down_next_to(L1, L2):-
    next_to(L1, L2),
    sp_location(L1, [_, Y1]),
    sp_location(L2, [_, Y2]),
    Y1 > Y2.

% all superpixels surrounds L
surround(L, S):-
    findall(X, next_to(L, X), S).

