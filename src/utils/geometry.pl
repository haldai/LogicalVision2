%=====================
% library for geometry
%=====================
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
