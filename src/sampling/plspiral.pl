%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% predicates about spiral
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
spiral_order(1.5).
spiral_step(5e-2).
% archimedian spiral points
spiral_points_2d([X, Y | Z], [W, H | D], [A, B], Re):-
    spiral_order(O), spiral_step(Step),
    spiral_points_2d([X, Y | Z], [W, H | D], [A, B], [O, Step], 0, [], Re), !.
spiral_points_2d([X, Y | _], [W, H | _], [A, B], [O, Step], I, _, []):-
    compute_spiral_points_2d([X, Y], [A, B], [O, Step], I, [XX, YY]),
    max_dist_2d([X, Y], [W, H], MaxD),
    Dist is sqrt((X - XX)**2 + (Y - YY)**2), % if the spiral is too large
    Dist > MaxD, !. % end this process
spiral_points_2d([X, Y | Z], [W, H | D], [A, B], [O, Step], I, Temp, [Temp1 | Re]):-
    compute_spiral_points_2d([X, Y], [A, B], [O, Step], I, [XX, YY]),
    % out of canvas
    out_of_canvas_2d([XX, YY], [W, H]),
    Temp \= [],
    %list_to_set(Temp, Temp1),
    Temp = Temp1,
    I1 is I + 1,
    spiral_points_2d([X, Y | Z], [W, H | D], [A, B], [O, Step], I1, [], Re), !.
spiral_points_2d([X, Y | Z], [W, H | D], [A, B], [O, Step], I, [], Re):-
    compute_spiral_points_2d([X, Y], [A, B], [O, Step], I, [XX, YY]),
    % out of canvas
    out_of_canvas_2d([XX, YY], [W, H]),
    I1 is I + 1,
    spiral_points_2d([X, Y | Z], [W, H | D], [A, B], [O, Step], I1, [], Re), !.
spiral_points_2d([X, Y | Z], [W, H | D], [A, B], [O, Step], I, Temp, Re):-
    compute_spiral_points_2d([X, Y], [A, B], [O, Step], I, [XX, YY]),    
    I1 is I + 1,
    % (Temp \= [] ->
    %      (last(Temp, [LX, LY | Z]),
    %       line_seg_points([LX, LY | Z], [XX, YY | Z], [_ | Ps]),
    %       append(Temp, Ps, Temp1), !);
    %  (append(Temp, [[XX, YY | Z]], Temp1), !)
    % ),
    append(Temp, [[XX, YY | Z]], Temp1),
    spiral_points_2d([X, Y | Z], [W, H | D], [A, B], [O, Step], I1, Temp1, Re), !.

% compute next spiral point
compute_spiral_points_2d([X, Y], [A, B], [O, Step], I, [XX, YY]):-
    Angle is Step * I,
    XX is round(X + (A + B * (Angle**O)) * cos(Angle)),
    YY is round(Y + (A + B * (Angle**O)) * sin(Angle)).

% maximum distance from [X, Y] to any point on canvas (range [0~W-1,0~H-1])
max_dist_2d([X, Y | _], [W, H | _], D):-
    D1 is sqrt(X**2 + Y**2),
    D2 is sqrt((W - X - 1)**2 + Y**2),
    D3 is sqrt(X**2 + (H - 1 - Y)**2),
    D4 is sqrt((W - X - 1)**2 + (H - 1 - Y)**2),
    max_list([D1, D2, D3, D4], D).

% out of 2d canvas
out_of_canvas_2d(P, Box):-
    out_box(Box, P).

%===========================
% random spiral sampling
%===========================
rand_spiral_sampling([_, _], [_, _], 0, []):-
    !.
rand_spiral_sampling([W, H], [A, B], N, Segs):-
    random(0, W, X), random(0, H, Y),
    spiral_points_2d([X, Y], [W, H], [A, B], S),
    N1 is N - 1,
    rand_spiral_sampling([W, H], [A, B], N1, S1),
    append(S, S1, Segs).
