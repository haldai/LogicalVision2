/* Sampling module (region based)
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

square_rad_min(5).

%%%%%%%%%% spiral sampling of the regions %%%%%%%%%%%%%%
spiral_regions_2d([X, Y | Z], [W, H | D], [A, B], [O, Step], Regions):-
    spiral_points_2d([X, Y | Z], [W, H | D], [A, B], [O, Step], 0, [], Centers),
    center_lists_to_regions_2d(Centers, LRegions),
    append(LRegions, Regions).

rand_spiral_regions_2d([_, _], [_, _], [_, _], 0, []):-
    !.
rand_spiral_regions_2d([W, H], [A, B], [Order, Step], Num, Regions):-
    Num > 0,
    random(0, W, X), random(0, H, Y),
    spiral_regions_2d([X, Y], [W, H], [A, B], [Order, Step], R),
    N is Num - 1,
    rand_spiral_regions_2d([W, H], [A, B], [Order, Step], N, R1),
    append(R, R1, Regions).

/* center lists: Ls = [[[X11, Y11], [X12, Y12], ...],
 *                    [[X21, Y21], ...],
 *                    ...]
 * Regions: R = [[X, Y], Radius]
 */
center_lists_to_regions_2d([], []):- !.
center_lists_to_regions_2d([L | Ls], [R | Rs]):-
    centers_to_regions_2d(L, R),
    center_lists_to_regions_2d(Ls, Rs).
centers_to_regions_2d([_], []):- !.
centers_to_regions_2d([C1, C2], [R1, R2]):-
    C1 = [X1, Y1 | Z], C2 = [X2, Y2 | Z],
    Radius1 is round(sqrt((X1 - X2)**2 + (Y1 - Y2)**2)/3),
    square_rad_min(RMIN),
    (Radius1 =< RMIN -> Radius = RMIN; Radius = Radius1), !,
    R1 = [C1, Radius], R2 = [C2, Radius], % regions
    !.
centers_to_regions_2d([C1, C2 | Cs], [R | Rs]):-
    C1 = [X1, Y1 | Z], C2 = [X2, Y2 | Z],
    Radius1 is round(sqrt((X1 - X2)**2 + (Y1 - Y2)**2)/3),
    square_rad_min(RMIN),
    (Radius1 =< RMIN -> Radius = RMIN; Radius = Radius1), !,
    R = [C1, Radius],
    centers_to_regions_2d([C2 | Cs], Rs).

