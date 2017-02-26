/*************************************************************************
This file is part of Logical Vision 2.

Logical Vision 2 is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Logical Vision 2 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Logical Vision 2.  If not, see <http://www.gnu.org/licenses/>.
************************************************************************/
%%%%%%%%%%%%%%%%%%%%%%
% POLYGON ABDUCTION
%%%%%%%%%%%%%%%%%%%%%%
edge_turns(3).
edge_scharr_2d(20.0).
edge_scharr(7).
same_line_angle_thresh(0.1).

%============
% conjecture
%============
conjecture_edge_2d(_, _, [], []):-
    !.
conjecture_edge_2d(Img, Points, [[P1, P2] | Conjs], [[P1, P2] | Edgs]):-
    edge_seg_2d(Img, [P1, P2]),
    remove_points_on_seg([P1, P2], Points, Points1),
    remove_edge_conj(Conjs, Points1, Conjs1), % remove conjectures 
    conjecture_edge_2d(Img, Points1, Conjs1, Edgs), !.
conjecture_edge_2d(Img, Points, [[P1, P2] | Conjs], Edgs):-
    sample_line_cross_seg_2d([P1, P2], Line),
    sample_edge_pts_2d(Img, Line, Edg_pts),
    new_conjs(Edg_pts, Points, Conjs1), % make new conjectures
    append(Conjs, Conjs1, Conjs2),
    conjecture_edge_2d(Img, Points, Conjs2, Edgs), !.

%=============
% primitives
%=============
% determine whether an segment is an edge
edge_seg_2d(Img, [P1, P2]):-
    edge_seg_2d(Img, [P1, P2], 3).
edge_seg_2d(_, _, 0):-
    !.
edge_seg_2d(Img, [P1, P2], N):-
    mid_point(P1, P2, P3),
    sample_point_scharr_2d(Img, P1, G1),
    sample_point_scharr_2d(Img, P2, G2),
    sample_point_scharr_2d(Img, P3, G3),
    edge_scharr(T),
    G1 >= T, G2 >= T, G3 >= T,
    N1 is N - 1,
    edge_seg_2d(Img, [P1, P3], N1),
    edge_seg_2d(Img, [P3, P2], N1), !.

% sample a line crosses a line segment
sample_line_cross_seg_2d([[X1, Y1], [X2, Y2]], Line):-
    sample_grid_lines_cross_seg_2d([[X1, Y1, 0], [X2, Y2, 0]], 2, Lines),
    Lines = [[[X, Y, 0], [DX, DY, 0]] | _],
    Line = [[X, Y], [DX, DY]].

% remove points on an valid edge segment
remove_points_on_seg(_, [], []):-
    !.
remove_points_on_seg([P1, P2], [P | Pts], Pts1):-
    vec_diff(P1, P, A), vec_diff(P, P2, B),
    vec_angle(A, B, Ang),
    same_line_angle_thresh(T), Ang =< T,
    remove_points_on_seg([P1, P2], Pts, Pts1), !.
remove_points_on_seg([P1, P2], [P | Pts], [P | Pts1]):-
    remove_points_on_seg([P1, P2], Pts, Pts1), !.

% remove conjectures containing invalid points
remove_edge_conj([], _, []):-
    !.
remove_edge_conj([C | Conjs], Pts, [C | Conjs1]):-
    subtract(C, Pts, []),
    remove_edge_conj(Conjs, Pts, Conjs1), !.
remove_edge_conj([_ | Conjs], Pts, Conjs1):-
    remove_edge_conj(Conjs, Pts, Conjs1), !.

% remove points that on the border of canvas
remove_border_pts_2d(_, [], []):-
    !.
remove_border_pts_2d([W, H], [P | Ps], Qs):-
    W1 is W - 1, H1 is H - 1,
    (P = [0, _]; P = [_, 0]; P = [W1, _]; P = [_, H1]), !,
    remove_border_pts_2d([W, H], Ps, Qs).
remove_border_pts_2d([W, H], [P | Ps], [P | Qs]):-
    remove_border_pts_2d([W, H], Ps, Qs).

% remove null conjectures
remove_null_edge_conjs_2d([], []):-
    !.
remove_null_edge_conjs_2d([[A, A] | Conjs], Re):-
    remove_null_edge_conjs_2d(Conjs, Re), !.
remove_null_edge_conjs_2d([[A, B] | Conjs], [[A, B] | Re]):-
    A \= B, remove_null_edge_conjs_2d(Conjs, Re), !.

% make new conjectures
new_conjs(Pts1, Pts2, Conjs):-
    cartesian_product(Pts1, Pts2, Conjs1),
    remove_null_edge_conjs_2d(Conjs1, Conjs).

% sample edge points
sample_edge_pts_2d(Img, [Point, Dir], Pts):-
    edge_scharr_2d(Th),
    line_pts_scharr_geq_T_2d(Img, Point, Dir, Th, Pts).

sample_edge_pts_2d(Img, [], Pts, Tmp):-
    size_2d(Img, W, H),
    remove_border_pts_2d([W, H], Tmp, Pts), !.
sample_edge_pts_2d(Img, [L | Ls], Pts, Tmp):-
    sample_edge_pts_2d(Img, L, P),
    append(Tmp, P, Tmp1),
    sample_edge_pts_2d(Img, Ls, Pts, Tmp1).
