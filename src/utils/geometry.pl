%=====================
% library for geometry
%=====================

% cross/3: Cross product of two 3d vectors
cross([A1, A2, A3], [B1, B2, B3], [X, Y, Z]):-
    X is A2*B3 - A3*B2,
    Y is A3*B1 - A1*B3,
    Z is A1*B2 - A2*B1.

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

% direction of vector P1P2 and P1P3, it is a determination of P1P3 and P1P2
vector_direction(P1, P2, P3, D):-
    P1 = [X1, Y1],
    P2 = [X2, Y2],
    P3 = [X3, Y3],
    D is (X3 - X1)*(Y2 - Y1) - (Y3 - Y1)*(X2 - X1).

% point is on segment
on_segment(P1, P2, P3):-
    P1 = [X1, Y1],
    P2 = [X2, Y2],
    P3 = [X3, Y3],
    (X1 < X2 -> (X_min = X1, X_max = X2); (X_min = X2, X_max = X1)),
    (Y1 < Y2 -> (Y_min = Y1, Y_max = Y2); (Y_min = Y2, Y_max = Y1)),
    \+(X3 < X_min; X3 > X_max; Y3 < Y_min; Y3 > Y_max).

% inersection of two line segments
intersected_seg(S1, S2):-
    S1 = [P1, P2],
    S2 = [P3, P4],
    vector_direction(P3, P4, P1, D1),
    vector_direction(P3, P4, P2, D2),
    vector_direction(P1, P2, P3, D3),
    vector_direction(P1, P2, P4, D4),
    ((D1*D2 < 0, D3*D4 < 0, !);
     (D1 =:= 0, on_segment(P3, P4, P1), !);
     (D2 =:= 0, on_segment(P3, P4, P2), !);
     (D3 =:= 0, on_segment(P1, P2, P3), !);
     (D4 =:= 0, on_segment(P1, P2, P4), !)
    ).

% return intersected point
intersected_seg(S1, S2, Points):-
    intersected_seg(S1, S2) ->
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
    
