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
