%=====================
% library for geometry
%=====================

% mid point of two vectors (points)
mid_point([], [], []):-
    !.
mid_point([X | Xs], [Y | Ys], [Z | Zs]):-
    Z is round((X + Y)/2),
    mid_point(Xs, Ys, Zs), !.

% cross/3: Cross product of two 3d vectors
cross([A1, A2, A3], [B1, B2, B3], [X, Y, Z]):-
    X is A2*B3 - A3*B2,
    Y is A3*B1 - A1*B3,
    Z is A1*B2 - A2*B1.
% cross/2 cross product of two 2d vector
cross([X1, Y1], [X2, Y2], C):-
    C is X1*Y2 - X2*Y1.

% dot/3: Inner product of two lists (vectors)
dot([], [], 0).
dot([X | Xs], [Y | Ys], Result):-
    Prod is X*Y,
    dot(Xs, Ys, Remaining),
    Result is Prod + Remaining.

% eu_dist/3: Euclidean distance between two vectors
eu_dist(A, B, D):-
    eu_dist_sum(A, B, S),
    D is sqrt(S).
eu_dist_sum([], [], 0).
eu_dist_sum([X | Xs], [Y | Ys], Sum):-
    Dist is (X - Y)**2,
    eu_dist_sum(Xs, Ys, Remaining),
    Sum is Dist + Remaining.

% seg_length/2: Length of a line segment
seg_length([], 0):-
    !.
seg_length(S, L):-
    S = [X, Y],
    eu_dist(X, Y, L), !.

%===================
% vector angle (DEG)
%===================
vec_angle(V1, V2, 180.0):-
    V1 = [X, Y], V2 = [U, V],
    X =:= -U,
    Y =:= -V,
    !.
vec_angle(V1, V2, Ang):-
    dot(V1, V2, D),
    norm_2(V1, N1), norm_2(V2, N2),
    N1 =\= 0, N2 =\= 0,
    A is D / (N1 * N2),
    Ang is round(((acos(A)*180) / pi)*1e5) / 1e5,
    !.
vec_angle(_, _, 0):-
    !.
% rotation angle, from 0 to 360 degree
vec_rotate_angle(V1, V2, 180.0):-
    V1 = [X, Y], V2 = [U, V],
    X =:= -U,
    Y =:= -V,
    !.
vec_rotate_angle(V1, V2, Ang):-
    cross(V1, V2, C),
    vec_angle(V1, V2, Ang1),
    Ang is Ang1*sign(C),
    !.

vec_rotate_angle_clockwise(V1, V2, Ang):-
    vec_rotate_angle(V1, V2, Ang1),
    Ang1 < 0,
    Ang is Ang1 + 360, !.
vec_rotate_angle_clockwise(V1, V2, Ang):-
    vec_rotate_angle(V1, V2, Ang), !.

%===================
% rectangle area
%===================
% [X1, Y1], [X2, Y2] are top-left and bottom-right points
rect_area([[X1, Y1], [X2, Y2]], S):-
    S is (X2 - X1) * (Y2 - Y1).
rect_intsct_area([[X1, Y1], [X2, Y2]], [[X3, Y3], [X4, Y4]], S):-
    S is max(0, min(X2, X4) - max(X1, X3)) * max(0, min(Y2, Y4) - max(Y1, Y3)).
    
%===============================
% point in/out box (rectangle)
%===============================
% point3 is in the box of [P1, P2]. P1 & P2 are ends of diagonal line segment.
in_box([P1, P2], P3):-
    P1 = [X1, Y1],
    P2 = [X2, Y2],
    P3 = [X3, Y3],
    (X1 < X2 -> (X_min = X1, X_max = X2, !); (X_min = X2, X_max = X1, !)),
    (Y1 < Y2 -> (Y_min = Y1, Y_max = Y2, !); (Y_min = Y2, Y_max = Y1, !)),
    X3 >= X_min,
    X3 =< X_max,
    Y3 >= Y_min,
    Y3 =< Y_max.

in_box([P1, P2], P3):-
    P1 = [X1, Y1, Z1],
    P2 = [X2, Y2, Z2],
    P3 = [X3, Y3, Z3],
    (X1 < X2 -> (X_min = X1, X_max = X2, !); (X_min = X2, X_max = X1, !)),
    (Y1 < Y2 -> (Y_min = Y1, Y_max = Y2, !); (Y_min = Y2, Y_max = Y1, !)),
    (Z1 < Z2 -> (Z_min = Z1, Z_max = Z2, !); (Z_min = Z2, Z_max = Z1, !)),
    X3 >= X_min,
    X3 =< X_max,
    Y3 >= Y_min,
    Y3 =< Y_max,
    Z3 >= Z_min,
    Z3 =< Z_max.

% out of box
%out_box(Box, Point)
out_box([W, H], [X, Y]):-
    (X > W - 1;
     Y > H - 1;
     X < 0;
     Y < 0),
    !.

out_box([W, H, D], [X, Y, Z]):-
    (X > W - 1;
     Y > H - 1;
     Z > D - 1;
     X < 0;
     Y < 0;
     Z < 0),
    !.

% count number of points in box
points_in_box(_, [], 0):-
    !.
points_in_box(Box, [P | Ps], N):-
    in_box(Box, P),
    points_in_box(Box, Ps, N1),
    N is N1 + 1, !.
points_in_box(Box, [_ | Ps], N):-
    points_in_box(Box, Ps, N), !.

%===================
% interesection 2d
%===================
% inersection of two 2d line segments
intersected_seg_2d(S1, S2):-
    S1 = [P1, P2],
    S2 = [P3, P4],
    intersect_det(P3, P4, P1, D1),
    intersect_det(P3, P4, P2, D2),
    intersect_det(P1, P2, P3, D3),
    intersect_det(P1, P2, P4, D4),
    ((D1*D2 < 0, D3*D4 < 0, !);
     (D1 =:= 0, in_box([P3, P4], P1), !);
     (D2 =:= 0, in_box([P3, P4], P2), !);
     (D3 =:= 0, in_box([P1, P2], P3), !);
     (D4 =:= 0, in_box([P1, P2], P4), !)
    ).

% return intersected point
intersected_seg_2d(S1, S2, Points):-
    intersected_seg_2d(S1, S2) ->
	(line_parameters(S1, A1, B1, C1),
	 line_parameters(S2, A2, B2, C2),
	 D is A1*B2 - A2*B1,
	 (D =:= 0 ->
	      (sample_line_seg(S1, PL1),
	       sample_line_seg(S2, PL2),
	       findall(P, (member(P, PL1), member(P, PL2)), Points),
	       !
	      );
	  (Dx is -(C1*B2 - C2*B1),
	   Dy is (C1*A2 - C2*A1),
	   X is truncate(Dx/D + 0.5),
	   Y is truncate(Dy/D + 0.5),
	   Points = [[X, Y]],
	   !
	  )
	 ),
	 !
	);
    (Points = [], !).

% determination of P1P3 and P1P2
intersect_det(P1, P2, P3, D):-
    P1 = [X1, Y1],
    P2 = [X2, Y2],
    P3 = [X3, Y3],
    D is (X3 - X1)*(Y2 - Y1) - (Y3 - Y1)*(X2 - X1).

%====================================
% 2d direction turning (clockwise)
%====================================
turn_degree_2d(Dir1, Deg_, Dir2):-
    Deg1 is round((Deg_ - truncate(Deg_/360)*360)*10)/10, % for robustness
    Deg is (Deg1*pi)/180, % DEG to RAD
    turn_degree_2d_(Dir1, Deg, Dir2), !.
turn_degree_2d_([DX, DY], Deg, [DX2, DY2]):-
    % Because in IMAGES, THE (0, 0) is on the TOP LEFT,
    %   so in fact here Deg is treated as INVERSE CLOCKWISE angle
    DX2 is round((DX*cos(Deg) - DY*sin(Deg))*10e6)/10e6,
    DY2 is round((DX*sin(Deg) + DY*cos(Deg))*10e6)/10e6.
    
%===========================
% perpendicular direction
%===========================
perpendicular([0, _], [1, 0]):- !.
perpendicular([_, 0], [0, 1]):- !.
perpendicular([X1, Y1], [10, Y2]):-
    Y2 is -10*X1/Y1.

perpendicular_bisect_line(Seg, Pt, Dir):-
    Seg = [P1, P2], mid_point(P1, P2, Pt),
    vec_diff(P1, P2, Dir0),
    perpendicular(Dir0, Dir).

%==============================================
% David Eberly's Segment intersection algorithm
%==============================================
% seg to seg distance
seg_seg_distance([P0, P1], [Q0, Q1], Dist, Closest):-
    seg_seg_calc_param([P0, P1], [Q0, Q1],
                   [A, B, C, D, E],
                   [F00, F10, F01, F11, G00, G10, G01, G11]),
    ((A > 0, C > 0) ->
         (get_clamped_root(A, F00, F10, SValue0),
          get_clamped_root(A, F01, F11, SValue1),
          ((SValue0 =< 0, Classify0 = -1, !);
           (SValue0 >= 1, Classify0 = 1, !);
           (Classify0 = 0, !)),
          ((SValue1 =< 0, Classify1 = -1, !);
           (SValue1 >= 1, Classify1 = 1, !);
           (Classify1 = 0, !)),
          ((Classify0 == -1, Classify1 == -1) ->
               (RParam0 = 0,
                get_clamped_root(C, G00, G01, RParam1), !);
           ((Classify0 == 1, Classify1 == 1) ->
                (RParam0 = 1,
                 get_clamped_root(C, G10, G11, RParam1), !);
            (seg_seg_compute_intersection([A, B, C, D, E],
                                          [F00, F10, F01, F11,
                                           G00, G10, G01, G11],
                                          [SValue0, SValue1],
                                          [Classify0, Classify1],
                                          Edge, End),
             seg_seg_compute_min_param([A, B, C, D, E],
                                       [F00, F10, F01, F11,
                                        G00, G10, G01, G11],
                                       Edge, End, [RParam0, RParam1]),
             !)
           )
          )
         );
     (A > 0 ->
          (get_clamped_root(A, F00, F10, RParam0),
           RParam1 = 0, !);
      (C > 0 ->
           (RParam0 = 0,
           get_clamped_root(C, G00, G01, RParam1), !);
       (RParam0 = 0, RParam1 = 0, !)
      )
     )
    ),
    % closest 0
    RP0 is 1 - RParam0,
    vec_multiply(RP0, P0, C00),
    vec_multiply(RParam0, P1, C01),
    vec_sum(C00, C01, Closest0),
    % closest 1
    RP1 is 1 - RParam1,
    vec_multiply(RP1, Q0, C10),
    vec_multiply(RParam1, Q1, C11),
    vec_sum(C10, C11, Closest1),
    Closest = [Closest0, Closest1],
    % Distance
    eu_dist(Closest0, Closest1, Dist).

seg_seg_calc_param([P0, P1], [Q0, Q1],
                   [A, B, C, D, E],
                   [F00, F10, F01, F11, G00, G10, G01, G11]):-
    vec_diff(P1, P0, P10),
    vec_diff(Q1, Q0, Q10),
    vec_diff(P0, Q0, PQ0),
    dot(P10, P10, A), dot(P10, Q10, B),  dot(Q10, Q10, C),
    dot(P10, PQ0, D), dot(Q10, PQ0, E),
    
    F00 is D,
    F10 is F00 + A,
    F01 is F00 - B,
    F11 is F10 - B,
    
    G00 is -E,
    G10 is G00 - B,
    G01 is G00 + C,
    G11 is G10 + C.

get_clamped_root(Slope, H0, H1, Root):-
    (H0 < 0 ->
         (H1 > 0 ->
              (R is -H0/Slope,
               (R > 1 ->
                    (Root is 0.5, !);
                (Root is R, !)
               )
              );
          (Root is 1, !)
         );
     (Root is 0, !)
    ).

seg_seg_compute_intersection([_, B, _, _, _],
                             [F00, F10, _, _, _, _, _, _],
                             [SV0, SV1], [CLS0, CLS1],
                             [EDG0, EDG1], [END0, END1]):-
    (CLS0 < 0 ->
         (EDG0 is 0,
          END00 is 0,
          END01_ is F00/B,
          ((END01_ < 0; END01_ > 1) ->
               (END01 is 0.5, !);
           (END01 = END01_, !)
          ),
          (CLS1 == 0 ->
               (EDG1 is 3,
                END10 is SV1,
                END11 is 1, !);
           (EDG1 is 1,
            END10 is 1,
            END11_ is F10/B,
            ((END11_ < 0; END11_ > 1) ->
                 (END11 is 0.5, !);
             (END11 is END11_, !)
            )
           )
          )
         );
     (CLS0 == 0 ->
          (EDG0 is 2,
           END00 is SV0,
           END01 is 0,
           (CLS1 < 0 ->
                (EDG1 is 0,
                 END10 is 0,
                 END11_ is F00/B,
                 ((END11_ < 0; END11_ > 1) ->
                      (END11 is 0.5, !);
                  (END11 is END11_, !)
                 )
                );
            (CLS1 == 0 ->
                 (EDG1 is 3,
                  END10 is SV1,
                  END11 is 1, !);
             (EDG1 is 1,
              END10 is 1,
              END11_ is F10/B,
              ((END11_ < 0; END11_ > 1) ->
                   (END11 is 0.5, !);
               (END11 is END11_, !)
              )
             )
            )
           ), !);
      (EDG0 is 1,
       END00 is 1,
       END01_ is F10/B,
       ((END01_ < 0; END01_ > 1) ->
            (END01 is 0.5, !);
        (END01 is END01_, !)
       ),
       (CLS1 == 0 ->
            (EDG1 is 3,
             END10 is SV1,
             END11 is 1, !);
        (EDG1 is 0,
         END10 is 0,
         END11_ is F00/B,
         ((END11_ < 0; END11_ > 1) ->
              (END11 is 0.5, !);
          (END11 is END11_, !)
         )
        )
       )
      )
     )
    ),
    END0 = [END00, END01],
    END1 = [END10, END11].

seg_seg_compute_min_param([_, B, C, _, E],
                          [_, _, _, _, G00, G10, G01, G11],
                          [EDG0, EDG1],
                          [[END00, END01], [END10, END11]],
                          [Para0, Para1]):-
    Delta is END11 - END01,
    H0 is Delta*(-B*END00 + C*END01 - E),
    (H0 >= 0 ->
         (EDG0 == 0 ->
              (Para0 is 0,
               get_clamped_root(C, G00, G01, Para1), !);
          (EDG0 == 1 ->
               (Para0 is 1,
                get_clamped_root(C, G10, G11, Para1), !);
           (Para0 is END00,
            Para1 is END01, !)
          )
         );
     (H1 is Delta*(-B*END10 + C*END11 - E),
      (H1 =< 0 ->
           (EDG1 == 0 ->
                (Para0 is 0,
                 get_clamped_root(C, G00, G01, Para1), !);
            (EDG1 == 1 ->
                 (Para0 is 1,
                  get_clamped_root(C, G10, G11, Para1), !);
             (Para0 is END10,
              Para1 is END11, !)
            )
           );
       (Z0 is H0/(H0 - H1),
        max_list([Z0, 0], Z1),
        min_list([Z1, 1], Z),
        OMZ is 1 - Z,
        Para0 is OMZ*END00 + Z*END10,
        Para1 is OMZ*END01 + Z*END11,
        !)
      )
     )
    ).

%=============================
% get a point on line segment
%=============================
point_on_seg([[X1, Y1, Z1], [X2, Y2, Z2]], T, [X, Y, Z]):-
    X is round(X1 + T*(X2 - X1)),
    Y is round(Y1 + T*(Y2 - Y1)),
    Z is round(Z1 + T*(Z2 - Z1)).
point_on_seg([[X1, Y1], [X2, Y2]], T, [X, Y]):-
    X is round(X1 + T*(X2 - X1)),
    Y is round(Y1 + T*(Y2 - Y1)).
rand_point_on_seg(Seg, Point):-
    random(T),
    point_on_seg(Seg, T, Point), !.

section_on_seg(Seg, N, Total, Point):-
    N >= 0, N =< Total,
    T is N / Total,
    point_on_seg(Seg, T, Point), !.

%===================================
% random 2d angle's direction vector
%===================================
rand_2d_angle_vec([X, Y]):-
    random(R), Phi is R*2*pi,
    X is cos(Phi),
    Y is sin(Phi).

%===================
% random 2d point
%===================
rand_2d_point([X, Y], [W, H]):-
    random(0, W, X),
    random(0, H, Y).

rand_3d_point([X, Y, Z], [W, H, D]):-
    random(0, W, X),
    random(0, H, Y),
    random(0, D, Z).

%===============
% random lines
%===============
rand_line_2d([W, H], [X, Y], [DX, DY]):-
    rand_2d_point([X, Y], [W, H]), rand_2d_angle_vec([DX, DY]).
% many lines
rand_lines_2d(_, 0, []):-
    !.
rand_lines_2d([W, H], N, [[[X, Y], [DX, DY]] | Lines]):-
    rand_2d_point([X, Y], [W, H]), rand_2d_angle_vec([DX, DY]),
    N1 is N - 1,
    rand_lines_2d([W, H], N1, Lines).

%======================
% point inside ellipse
%======================
point_in_ellipse(Pt, [Center, Param], _):-
    point_in_ellipse(Pt, [Center, Param]).
point_in_ellipse(Pt, [Center, Param]):-
    Pt = [X, Y],
    Center = [XC, YC], Param = [A, B, ALPHA],
    Th is pi*ALPHA/180,
    U is cos(Th)*(X - XC) + sin(Th)*(Y - YC),
    V is -sin(Th)*(X - XC) + cos(Th)*(Y - YC),
    D is (U/A)**2 + (V/B)**2,
    D =< 1.01.
/*
line_seg_points(Pt, Center, [W, H, D], Pts1),
ellipse_points(Center, Param, [W, H, D], Pts2),
intersection(Pts1, Pts2, Pts3),
length(Pts3, L),
(member(Pt, Pts2);
L == 0),
!.
*/

%======================
% point inside circle
%======================
point_in_circle(Pt, [Center, Radius], _):-
    point_in_circle(Pt, [Center, Radius]).
point_in_circle(Pt, [Center, Radius]):-
    eu_dist(Pt, Center, D),
    D =< Radius.

%====================
% points in ellipse
%====================
% get_points_in_ellipse_2d([Center, Parameters], Bound, Return).
get_points_in_ellipse_2d([Cen, Param], [W, H], Return):-
    ellipse_points_2d(Cen, Param, [W, H], Edge),
    column(1, Edge, Xs), column(2, Edge, Ys),
    min_list(Xs, Xmin), max_list(Xs, Xmax),
    min_list(Ys, Ymin), max_list(Ys, Ymax),
    findall([X, Y],
            (between(Xmin, Xmax, X),
             between(Ymin, Ymax, Y),
             point_in_ellipse([X, Y], [Cen, Param])
            ),
            Return
           ).

%====================
% points in circle
%====================
% get_points_in_circle_2d([Center, Radius], Bound, Return).
get_points_in_circle_2d([Cen, Radius], [W, H], Return):-
    circle_points_2d(Cen, Radius, [W, H], Edge),
    column(1, Edge, Xs), column(2, Edge, Ys),
    min_list(Xs, Xmin), max_list(Xs, Xmax),
    min_list(Ys, Ymin), max_list(Ys, Ymax),
    findall([X, Y],
            (between(Xmin, Xmax, X),
             between(Ymin, Ymax, Y),
             point_in_circle([X, Y], [Cen, Radius])
            ),
            Return
           ).

%==========================
% point ellipse distance
%==========================
dist_elps_point_2d(Elps, Point, Dist):-
    dist_point_elps_2d(Point, Elps, Dist).
dist_point_elps_2d([], [_, _], 100000):-
    !.
dist_point_elps_2d(Point, [Cen, Param], Dist):-
    dist_point_elps_2d(Point, [Cen, Param], Dist, _).
dist_point_elps_2d(Point, [Cen, Param], Dist, Closest):-
    dist_point_elps_2d(Point, Cen, Param, Dist, Closest).

%==========================
% point circle distance
%==========================
dist_circle_point_2d(Circle, Point, Dist):-
    dist_point_circle_2d(Point, Circle, Dist).

dist_point_circle_2d([], [_, _], 100000):-
    !.
dist_point_circle_2d(Point, Circle, Dist):-
    dist_point_circle_2d(Point, Circle, Dist, _).

dist_point_circle_2d([PX, PY], [Cen, Radius], Dist, Closest):-
    eu_dist([PX, PY], Cen, D),
    Dist is abs(D - Radius),
    Cen = [CX, CY],
    X is round(CX + Radius*(PX - CX)/D),
    Y is round(CY + Radius*(PY - CY)/D),
    Closest = [X, Y].

%==============================
% segment ellipse intersection
%==============================
intsct_seg_elps([S, E], [Center, Param], Bound, Intsct):-
    line_seg_points(S, E, Bound, Pts1),
    ellipse_points(Center, Param, Bound, Pts2),
    intersection(Pts1, Pts2, Pts3),
    ((Pts3 = [], point_in_ellipse(S, [Center, Param], Bound), Intsct = [S, E]);
     (Pts3 = [], \+point_in_ellipse(S, [Center, Param], Bound), Intsct = []);
     (Pts3 = [A], point_in_ellipse(S, [Center, Param], Bound), Intsct = [A, S]);
     (Pts3 = [A], point_in_ellipse(E, [Center, Param], Bound), Intsct = [A, E]);
     (Pts3 = [A, B], Intsct = [A, B])),
    !.

%=========================
% random 2d lines sampling
%=========================
% randomly sample multiple lines
rand_sample_2d_lines(_, _, 0, []):-
    !.
rand_sample_2d_lines(Frame, [W, H], N, [[C, Dir] | CDs]):-
    W1 is W - 1, H1 is H - 1,
    random_between(0, W1, X), random_between(0, H1, Y),
    C = [X, Y, Frame],
    rand_2d_angle_vec([XX, YY]),
    Dir = [XX, YY, 0],
    N1 is N - 1,
    rand_sample_2d_lines(Frame, [W, H], N1, CDs).

rand_sample_2d_lines(_, 1, []):-
    !.
rand_sample_2d_lines([W, H], N, [[C, Dir] | CDs]):-
    W1 is W - 1, H1 is H - 1,
    random_between(0, W1, X), random_between(0, H1, Y),
    C = [X, Y],
    rand_2d_angle_vec([XX, YY]),
    Dir = [XX, YY],
    N1 is N - 1,
    rand_sample_2d_lines([W, H], N1, CDs).

% horizontally sample multiple lines (uniform distributed)
% sample_xxx_lines(+Frame, +[Width, Height], -Lines).
sample_horizon_lines(Frame, [_, H], N, Lines):-
    P is H / N,
    sample_horizon_lines(Frame, [_, H], N, P, Lines), !.

% horizontally sample multiple lines (uniform distributed)
% sample_xxx_lines(+Frame, +[Width, Height], -Lines).
sample_horizon_lines(Frame, [_, H], N, Lines):-
    P is H / N,
    sample_horizon_lines(Frame, [_, H], N, P, Lines), !.
sample_horizon_lines(_, _, 0, _, []):-
    !.
sample_horizon_lines(Frame, [_, H], N, P, [[[0, Y, Frame], [1, 0]] | Lines]):-
    Y is ceil(N * P),
    Y =\= 0,
    \+ minus(H, 1, Y),
    N1 is N - 1,
    sample_horizon_lines(Frame, [_, H], N1, P, Lines).
sample_horizon_lines(Frame, [_, H], N, P, Lines):-
    N1 is N - 1,
    sample_horizon_lines(Frame, [_, H], N1, P, Lines).

% vertically sample multiple lines (uniform distributed)
sample_vertical_lines(Frame, [W, _], N, Lines):-
    P is W / N,
    sample_vertical_lines(Frame, [W, _], N, P, Lines), !.
sample_vertical_lines(_, _, 0, _, []):-
    !.
sample_vertical_lines(Frame, [W, _], N, P, [[[X, 0, Frame], [0, 1]] | Lines]):-
    X is ceil(N * P),
    X =\= 0,
    \+ minus(W, 1, X),
    N1 is N - 1,
    sample_vertical_lines(Frame, [W, _], N1, P, Lines).
sample_vertical_lines(Frame, [W, _], N, P, Lines):-
    N1 is N - 1,
    sample_vertical_lines(Frame, [W, _], N1, P, Lines).

%=======================================================
% calculate the total length of a list of line segments
%=======================================================
total_seg_length([], 0):-
    !.
total_seg_length([[Start, End]-_ | Segs], Sum):-
    eu_dist(Start, End, Dist),
    total_seg_length(Segs, Sum1),
    !,
    Sum is Sum1 + Dist.
total_seg_length([[Start, End] | Segs], Sum):-
    eu_dist(Start, End, Dist),
    total_seg_length(Segs, Sum1),
    !,
    Sum is Sum1 + Dist.

%====================================
% closest point in a list of points
%====================================
closest_point_in_list(P, List, Re):-
    closest_point_in_list(P, List, [-100, -100], 10000, Re).
closest_point_in_list(_, [], Re, _, Re):-
    !.
closest_point_in_list(P, [I | L], _, Tmp_Dist, Re):-
    eu_dist(P, I, D),
    D < Tmp_Dist,
    closest_point_in_list(P, L, I, D, Re), !.
closest_point_in_list(P, [_ | L], Tmp, Tmp_Dist, Re):-
    closest_point_in_list(P, L, Tmp, Tmp_Dist, Re), !.
