/* Sampling module (region based)
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

spiral_regions_2d([X, Y | Z], [W, H | D], [A, B], [O, Step], Regions):-
    spiral_points_2d([X, Y | Z], [W, H | D], [A, B], [O, Step], 0, [], Centers),
    center_lists_to_regions_2d(Centers, LRegions),
    append(LRegions, Regions).

center_lists_to_regions_2d([], []):- !.
center_lists_to_regions_2d([L | Ls], [R | Rs]):-
    centers_to_regions_2d(L, R),
    center_lists_to_regions_2d(Ls, Rs).
centers_to_regions_2d([_], []):- !.
centers_to_regions_2d([C1, C2], [R1, R2]):-
    C1 = [X1, Y1 | Z], C2 = [X2, Y2 | Z],
    Radius1 is round(sqrt((X1 - X2)**2 + (Y1 - Y2)**2)/3),
    (Radius1 =< 30 -> Radius = 30; Radius = Radius1), !,
    R1 = [C1, Radius], R2 = [C2, Radius],
    !.
centers_to_regions_2d([C1, C2 | Cs], [R | Rs]):-
    C1 = [X1, Y1 | Z], C2 = [X2, Y2 | Z],
    Radius1 is round(sqrt((X1 - X2)**2 + (Y1 - Y2)**2)/3),
    (Radius1 =< 30 -> Radius = 30; Radius = Radius1), !,
    R = [C1, Radius],
    centers_to_regions_2d([C2 | Cs], Rs).
