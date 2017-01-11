/* Drawing module
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

:- load_foreign_library(foreign('../../libs/cvdraw.so')).

%============================================
% draw line (not line segment, no boundary)
%============================================
draw_line_2d(Image, Point, Dir, Color):-
    size_2d(Image, W, H),
    line_points(Point, Dir, [W, H, 10e300], Pts),
    nth1(1, Pts, St), last(Pts, Lst), % get start and end points
    draw_line_seg_2d(Image, St, Lst, Color). % use opencv line drawing
    
draw_line(Imgseq, Point, Dir, Color):-
    size_3d(Imgseq, W, H, D),
    line_points(Point, Dir, [W, H, D], Pts),
    draw_points(Imgseq, Pts, Color). % draw points

%=========================
% draw line segments 2d
%=========================
draw_line_segs_2d(_, [], _):-
    !.
draw_line_segs_2d(Img, [[S, E] | Ss], Color):-
    draw_line_seg_2d(Img, S, E, Color),
    draw_line_segs_2d(Img, Ss, Color).

%==================
% draw ellipses (2D)
%==================
draw_elpses(_, []):-
    !.
draw_elpses(Img, [elps(Cen, Para, C) | Elpses]):-
    size_2d(Img, W, H),
    ellipse_points(Cen, Para, [W, H, 1000000], Pts),
    ((C == 0, Col = r);
     (C == 1, Col = g);
     (C == 2, Col = b);
     (C == 3, Col = y);
     (C == 4, Col = k)),
    !,
    draw_points_2d(Img, Pts, Col),
    draw_elpses(Img, Elpses).

%=================
% draw rectangles
%=================
draw_rects(_, [], _):-!.
draw_rects(Imgseq, [R | Rs], Color):-
    R = [Cen, Rad],
    draw_rect(Imgseq, Cen, Rad, Color),
    draw_rects(Imgseq, Rs, Color).

draw_rects_2d(_, [], _):-!.
draw_rects_2d(Img, [R | Rs], Color):-
    R = [Cen, Rad],
    draw_rect_2d(Img, Cen, Rad, Color),
    draw_rects_2d(Img, Rs, Color).

draw_squares_2d(_, [], _):-!.
draw_squares_2d(Img, [S | Ss], Color):-
    S = [Cen, R],
    draw_rect_2d(Img, Cen, [R, R], Color),
    draw_squares_2d(Img, Ss, Color).

%=========================
% draw points with label
%=========================
draw_points_with_label_2d(_, []):-
    !.
draw_points_with_label_2d(Img, [P-L | Pts]):-
    draw_point_with_label_2d(Img, P-L),
    draw_points_with_label_2d(Img, Pts).

draw_point_with_label_2d(Img, Pt-0):-
    draw_points_2d(Img, [Pt], blue), !.
draw_point_with_label_2d(Img, Pt-1):-
    draw_points_2d(Img, [Pt], red), !.
