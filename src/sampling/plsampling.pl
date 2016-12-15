/* Sampling module
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */
:- load_foreign_library(foreign('../../libs/cvsampler.so')),
   load_foreign_library(foreign('../../libs/cvcluster.so')).

:- ensure_loaded(['../utils/utils.pl']),
   %ensure_loaded(['../sampling/plregion.pl']),
   ensure_loaded(['../sampling/plsegment.pl']),
   ensure_loaded(['../sampling/plspiral.pl']),
   ensure_loaded(['../sampling/plline.pl']).


%==========================
% 2d version line_points
%==========================
line_points_2d([X, Y], [DX, DY], [W, H], Pts):-
    line_points([X, Y, 0], [DX, DY, 0], [W, H, 1], Pts1),
    trim_2d(Pts1, Pts).

%=========================
% N-d points to 2d points
%=========================
trim_2d([], []):-
    !.
trim_2d([[X, Y | _] | Ps], [[X, Y] | Qs]):-
    trim_2d(Ps, Qs).

%==================================
% edge points on line to segments
%==================================
pts2segs([_], []):-
    !.
pts2segs([P1, P2 | Ps], [[P1, P2] | Ss]):-
    pts2segs([P2 | Ps], Ss).

%============================
% line seg points (no bound)
%============================
line_seg_points([X1, Y1, Z1], [X2, Y2, Z2], Pts):-
    max_list([X1, X2], W1), max_list([Y1, Y2], H1), max_list([Z1, Z2], D1),
    W is W1 + 1, H is H1 + 1, D is D1 + 1,
    line_seg_points([X1, Y1, Z1], [X2, Y2, Z2], [W, H, D], Pts), !.
line_seg_points([X1, Y1], [X2, Y2], Pts):-
    max_list([X1, X2], W1), max_list([Y1, Y2], H1),
    W is W1 + 1, H is H1 + 1,
    line_seg_points([X1, Y1, 0], [X2, Y2, 0], [W, H, 1], Pts1),
    trim_2d(Pts1, Pts), !.

%==========================
% color of a single point
%==========================
pt_color_2d(Img, Pt, LAB):-
    pts_color_2d(Img, [Pt], [LAB]).
