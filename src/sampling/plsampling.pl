/* Sampling module
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */
:- load_foreign_library(foreign('../../libs/cvsampler.so')).
   

:- ensure_loaded(['../utils/utils.pl',
                  %'../sampling/plregion.pl',
                  '../sampling/plsegment.pl',
                  '../sampling/plspiral.pl',
                  '../sampling/plline.pl']).

pixel_neighbor_size(10).

%============================================
% 2d version geometric primitives
%============================================
line_points_2d([X, Y | _], [DX, DY | _], [W, H | _], Pts):-
    line_points([X, Y, 0], [DX, DY, 0], [W, H, 1], Pts1),
    trim_2d(Pts1, Pts).

ray_points_2d([X, Y | _], [DX, DY | _], [W, H | _], Pts):-
    ray_points([X, Y, 0], [DX, DY, 0], [W, H, 1], Pts1),
    trim_2d(Pts1, Pts).

line_seg_points_2d([SX, SY | _], [EX, EY | _], [W, H | _], Pts):-
    line_seg_points([SX, SY, 0], [EX, EY, 0], [W, H, 1], Pts1),
    trim_2d(Pts1, Pts).

fit_elps_2d(Pts, Center, Param):-
    ext_3d(Pts, Pts3),
    fit_elps(Pts3, [X, Y, 0], Param),
    Center = [X, Y].
fit_circle_2d(Pts, [X, Y], R):-
    ext_3d(Pts, Pts3),
    fit_circle(Pts3, [X, Y, 0], R).
test_fit_elps_2d(Pts, Center, Param):-
    ext_3d(Pts, Pts3),
    test_fit_elps(Pts3, [X, Y, 0], Param),
    Center = [X, Y].

ellipse_points_2d([X, Y | _], Param, [W, H | _], Pts):-
    ellipse_points([X, Y, 0], Param, [W, H, 1], Pts3),
    trim_2d(Pts3, Pts).
circle_points_2d([X, Y], R, [W, H | _], Pts):-
    circle_points([X, Y, 0], R, [W, H, 1], Pts3),
    trim_2d(Pts3, Pts).

%=========================
% N-d points to 2d points
%=========================
trim_2d([], []):-
    !.
trim_2d([[X, Y | _] | Ps], [[X, Y] | Qs]):-
    trim_2d(Ps, Qs).
% extend 2d to 3d point sequence
ext_3d([], []):-
    !.
ext_3d([[X, Y] | Ps], [[X, Y, 0] | Qs]):-
    ext_3d(Ps, Qs).

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

%=========================================
% color hists of neighborhoods of points
%=========================================
% sample color histogram feature vector of squares
color_hist_square_2d(Img, [Cen, R], Hist):-
    color_hist_rect_2d(Img, Cen, [R, R], Hist).

color_L_hist_square_2d(Img, [Cen, R], Hist):-
    color_L_hist_rect_2d(Img, Cen, [R, R], Hist).

% multiple squares
color_hists_squares_2d(_, [], []):-
    !.
color_hists_squares_2d(Img, [S | Sqs], [H | Hists]):-
    color_hist_square_2d(Img, S, H),
    color_hists_squares_2d(Img, Sqs, Hists).

% pixel neighborhoods, colored image, 96 bins
pts_color_hists_2d(_, [], []):-
    !.
pts_color_hists_2d(Img, [P | Pts], [H | Hists]):-
    pixel_neighbor_size(R),
    color_hist_square_2d(Img, [P, R], H),
    pts_color_hists_2d(Img, Pts, Hists).
% brightness, 128 bins
pts_color_L_hists_2d(_, [], []):-
    !.
pts_color_L_hists_2d(Img, [P | Pts], [H | Hists]):-
    pixel_neighbor_size(R),
    color_L_hist_square_2d(Img, [P, R], H),
    pts_color_L_hists_2d(Img, Pts, Hists).

%========================================
% points L channel (brightness) average
%========================================
pts_color_L_avg_2d(Img, Pts, Re):-
    pts_color_L_sum_2d(Img, Pts, N, Sum),
    divide0(Sum, N, Re).

pts_color_L_sum_2d(_, [], 0, 0):-
    !.
pts_color_L_sum_2d(Img, [P | Pts], N, Sum):-
    size_2d(Img, W, H), P = [X, Y],
    X < W, Y < H, X >= 0, Y >= 0,
    point_color_2d(Img, P, [L, _, _]),
    pts_color_L_sum_2d(Img, Pts, N1, Sum1),
    N is N1 + 1,
    Sum is Sum1 + L, !.
pts_color_L_sum_2d(Img, [_ | Pts], N, Sum):-
    pts_color_L_sum_2d(Img, Pts, N, Sum), !.
