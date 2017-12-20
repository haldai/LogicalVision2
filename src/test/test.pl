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
along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
************************************************************************/
/* Test module
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

:- ensure_loaded(['../abduce/plabduce.pl',
                  '../abduce/bk_light.pl',
                  '../abduce/bk_ellipse.pl',
                  '../io/plio.pl',
                  '../sampling/plsampling.pl',
		  '../sampling/plsegment.pl',
		  '../sampling/plellipse.pl',
                  '../drawing/pldraw.pl',
                  '../utils/utils.pl',
                  '../learning/pllearning.pl']).

% test utilities
test_write_start(Name):-
    write('[TEST] '), write(Name), write('.'), nl.
test_write_done:-
    write('[DONE]'), nl,
    write('================'), nl.

% test load image sequence
test_load_imgseq(B):-
    test_write_start('load imgseq'),
    test_load_v(A),
    test_v2s(A, B),
    test_rel_v(A),
    test_write_done.

% test load video
test_load_v(A):-
    test_write_start('load video'),
    load_video('../../data/Protist.mp4', A),
    size_3d(A, X, Y, Z),
    write('W x H x D: '),
    write(X), write(' x '), write(Y), write(' x '), write(Z), nl,
    test_write_done.

% test video to image sequence
test_v2s(A, B):-
    test_write_start('video to image sequence'),
    video2imgseq(A, B),
    test_write_done.

% test release video
test_rel_v(A):-
    test_write_start('release video.'),
    release_video(A),
    test_write_done.

% test release image sequence
test_rel_s(A):-
    test_write_start('release image sequence.'),
    release_imgseq(A),
    test_write_done.

test_show2imgs(A):-
    test_write_start('release show 2 images.'),
    seq_img(A, 0, Img1),
    seq_img(A, 700, Img2),
    show2imgs_win(Img1, Img2, 'test'),
    test_write_done.

% test draw line (on the first frame)
test_draw_line_2d(IMGSEQ, Point, Dir, Color):-
    test_write_start('test draw line (2d)'),
    seq_img(IMGSEQ, 0, IMG1),
    Point = [_, _, 0], Dir = [_, _, 0], % frame chk
    clone_img(IMG1, IMG2),
    draw_line_2d(IMG2, Point, Dir, Color),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),
    test_write_done.

test_draw_lines_2d(IMGSEQ, Lines, Color):-
    test_write_start('test draw many lines (2d)'),
    seq_img(IMGSEQ, 0, IMG1),
    clone_img(IMG1, IMG2),
    test_draw_lines_2d_(IMG2, Lines, Color),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),
    test_write_done.

test_draw_lines_2d_(_, [], _):- !.
test_draw_lines_2d_(IMG, [L | Lines], Color):-
    L = [Point, Dir],
    Point = [_, _, 0], Dir = [_, _, 0], % frame chk
    draw_line_2d(IMG, Point, Dir, Color),
    test_draw_lines_2d_(IMG, Lines, Color), !.

% test draw line (on image sequence)
test_draw_line(IMGSEQ, Point, Dir, Color):-
    test_write_start('test draw line (3d)'),
    clone_seq(IMGSEQ, SEQ1),
    draw_line(SEQ1, Point, Dir, Color),
    showseq_win(SEQ1, 'debug'),
    release_imgseq(SEQ1),
    test_write_done.

test_draw_line_(IMGSEQ, Point, Dir, Color):-
    test_write_start('test draw line (3d)'),
    clone_seq(IMGSEQ, SEQ1),
    draw_line(SEQ1, Point, Dir, Color),
    seq_img(SEQ1, 0, IMG1),
    showimg_win(IMG1, 'debug'),
    release_imgseq(SEQ1),
    test_write_done.

% test draw line segment (on image sequence)
test_draw_line_seg(IMGSEQ, Start, End, Color):-
    test_write_start('test draw line segment (3d)'),
    clone_seq(IMGSEQ, SEQ1),
    draw_line_seg(SEQ1, Start, End, Color),
    showseq_win(SEQ1, 'debug'),
    release_imgseq(SEQ1),
    test_write_done.

% test draw ellipse (on the first frame)
test_draw_elps(IMGSEQ, [Center, [A, B, ALPHA]], COLOR):-
    test_write_start('draw ellipse'),
    seq_img(IMGSEQ, 0, IMG1),
    clone_img(IMG1, IMG2),
    size_3d(IMGSEQ, W, H, D),
    ellipse_points(Center, [A, B, ALPHA], [W, H, D], PTS),
    draw_points_2d(IMG2, PTS, COLOR),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),
    test_write_done.

% test fit ellipse (on the first frame)
test_fit_elps(IMGSEQ, Center, [A, B, ALPHA], COLOR):-
    test_write_start('fit ellipse'),
    seq_img(IMGSEQ, 0, IMG1),
    clone_img(IMG1, IMG2),
    size_3d(IMGSEQ, W, H, D),
    ellipse_points(Center, [A, B, ALPHA], [W, H, D], PTS),
    index_select([1, 30, 50, 70, 90, 110, 130, 150, 170, 190], PTS, PTS2),
    %PTS2 = PTS,
    fit_elps(PTS2, Cen2, Para2),
    write('fit parameters: '), write(Cen2), write(', '), write(Para2), nl,
    ellipse_points(Cen2, Para2, [W, H, D], PTS3),
    draw_points_2d(IMG2, PTS3, COLOR),
    draw_points_2d(IMG2, PTS2, green),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),
    test_write_done.

test_fit_circle(IMGSEQ, [X, Y, R], COLOR):-
    test_write_start('fit circle'),
    seq_img(IMGSEQ, 0, IMG1),
    clone_img(IMG1, IMG2),
    size_3d(IMGSEQ, W, H, D),
    circle_points([X, Y, 0], R, [W, H, D], PTS),
    %index_select([1, 30, 50, 70, 90, 110, 130, 150, 170, 190], PTS, PTS2),
    index_select([1, 30, 50], PTS, PTS2),
    %PTS2 = PTS,
    fit_circle(PTS2, Para2, R),
    write('fit parameters: '), write(Para2), nl,
    circle_points(Para2, R, [W, H, D], PTS3),
    draw_points_2d(IMG2, PTS3, COLOR),
    draw_points_2d(IMG2, PTS2, green),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),
    test_write_done.

% sample a line and its variance
test_sample_line_var(Imgseq, Pts, Vars):-
    test_write_start('sample line variance'),
    sample_line_var(Imgseq, [100, 100, 0], [10, -7, 0], Pts, Vars),
    print(Pts), nl,
    print(Vars), nl,
    seq_img(Imgseq, 0, IMG1),
    clone_img(IMG1, IMG2),
    draw_points_2d(IMG2, Pts, red),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),    
    test_write_done.

% sample a line and its color
test_sample_line_color(Imgseq, Pts, Color):-
    test_write_start('sample line color'),
    sample_line_color(Imgseq, [100, 100, 0], [10, -7, 0], Pts, Color),
    print(Pts), nl,
    print(Color), nl,
    seq_img(Imgseq, 0, IMG1),
    clone_img(IMG1, IMG2),
    draw_points_2d(IMG2, Pts, red),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),    
    test_write_done.

% sample a line and its color
test_sample_line_color_L(Imgseq, Pts, L):-
    test_write_start('sample line color - brightness'),
    % get points and brightness
    sample_line_color_L(Imgseq, [351, 147, 0], [1, 1, 0], Pts, L),
    print(Pts), nl,
    print(L), nl,
    items_key_geq_T(Pts, L, 200, HL), % highlight
    items_key_less_T(Pts, L, 120, SHD), % shadow
    seq_img(Imgseq, 0, IMG1),
    clone_img(IMG1, IMG2),
    draw_points_2d(IMG2, HL, red),
    draw_points_2d(IMG2, SHD, blue),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),    
    test_write_done.

% sample a line and brightness gradient
test_sample_line_L_grad(Imgseq, Point, Dir):-
    test_write_start('sample line gradient - brightness'),
    % get points and brightness
    sample_line_L_grad(Imgseq, Point, Dir, Pts, G),
    print(Pts), nl,
    print(G), nl,
    items_key_geq_T(Pts, G, 2, HL), % highlight
    items_key_less_T(Pts, G, -1, SHD), % shadow
    Point = [_, _, Frame],
    seq_img(Imgseq, Frame, IMG1),
    clone_img(IMG1, IMG2),
    draw_line_2d(IMG2, Point, Dir, white),
    draw_points_2d(IMG2, HL, red),
    draw_points_2d(IMG2, SHD, blue),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),    
    test_write_done.

test_sample_parallel_lines_L_grad(Imgseq, Frame, Dir):-
    test_write_start('sample parallel lines gradient - brightness'),
    % from [0,0,F] to [639,0,F]
    seq_img(Imgseq, Frame, IMG1),
    clone_img(IMG1, IMG2),
    test_sample_parallel_lines_L_grad_(Imgseq, IMG2, 639, Frame, Dir),
    showimg_win(IMG2, 'parallel gradients'),
    release_img(IMG2),
    test_write_done.
test_sample_parallel_lines_L_grad_(_, _, 0, _, _):-
    !.
test_sample_parallel_lines_L_grad_(Imgseq, IMG, X, Frame, Dir):-
    sample_line_L_grad(Imgseq, [X, 0, Frame], Dir, Pts, G),
    items_key_geq_T(Pts, G, 3, HL), % highlight
    items_key_less_T(Pts, G, -2, SHD), % shadow
    draw_points_2d(IMG, HL, red),
    draw_points_2d(IMG, SHD, blue),
    X1 is X - 1,
    test_sample_parallel_lines_L_grad_(Imgseq, IMG, X1, Frame, Dir).

% sample multiple lines and brightness gradients
test_multiple_sample_line_L_grad(Imgseq):-
    % Centers = [[351, 147, 0], [351, 147, 0], [351, 147, 0], [351, 147, 0], [351, 147, 0], [351, 147, 0], [351, 147, 0], [351, 147, 0], [351, 147, 0], [351, 147, 0], [351, 147, 0], [351, 147, 0]],
    Centers = [[100, 80, 0], [100, 80, 0], [100, 80, 0], [100, 80, 0], [100, 80, 0], [100, 80, 0], [100, 80, 0], [100, 80, 0], [100, 80, 0], [100, 80, 0], [100, 80, 0], [100, 80, 0]],
    %Centers = [[311, 147, 0], [312, 147, 0], [313, 147, 0], [314, 147, 0], [315, 147, 0], [316, 147, 0], [317, 147, 0], [318, 147, 0], [319, 147, 0], [320, 147, 0],
    %           [321, 147, 0], [322, 147, 0], [323, 147, 0], [324, 147, 0], [325, 147, 0], [326, 147, 0], [327, 147, 0], [328, 147, 0], [329, 147, 0], [330, 147, 0],
    %           [331, 147, 0], [332, 147, 0], [333, 147, 0], [334, 147, 0], [335, 147, 0], [336, 147, 0], [337, 147, 0], [338, 147, 0], [339, 147, 0], [340, 147, 0],
    %           [341, 147, 0], [342, 147, 0], [343, 147, 0], [344, 147, 0], [345, 147, 0], [346, 147, 0], [347, 147, 0], [348, 147, 0], [349, 147, 0], [350, 147, 0],
    %           [351, 147, 0], [352, 147, 0], [353, 147, 0], [354, 147, 0], [355, 147, 0], [356, 147, 0], [357, 147, 0], [358, 147, 0], [359, 147, 0], [360, 147, 0],
    %           [361, 147, 0], [362, 147, 0], [363, 147, 0], [364, 147, 0], [365, 147, 0], [366, 147, 0], [367, 147, 0], [368, 147, 0], [369, 147, 0], [370, 147, 0],
    %           [371, 147, 0], [372, 147, 0], [373, 147, 0], [374, 147, 0], [375, 147, 0], [376, 147, 0], [377, 147, 0], [378, 147, 0], [379, 147, 0], [380, 147, 0],
    %           [381, 147, 0], [382, 147, 0], [383, 147, 0], [384, 147, 0], [385, 147, 0], [386, 147, 0], [387, 147, 0], [388, 147, 0], [389, 147, 0], [390, 147, 0],
    %           [391, 147, 0], [392, 147, 0], [393, 147, 0], [394, 147, 0], [395, 147, 0], [396, 147, 0], [397, 147, 0], [398, 147, 0], [399, 147, 0], [400, 147, 0]],
    Directions = [[-2, -1, 0], [-1, -1, 0], [0, -1, 0], [-1, 0, 0], [-1, -2, 0], [-3, -1, 0], [-1, -3, 0], [2, -1, 0], [3, -1, 0], [1, -2, 0], [1, -3, 0], [1, -1, 0]],
    %Directions = [[2, 1, 0], [1, 1, 0], [0, 1, 0], [1, 0, 0], [1, 2, 0], [3, 1, 0], [1, 3, 0], [-2, 1, 0], [-3, 1, 0], [-1, 2, 0], [-1, 3, 0], [-1, 1, 0]],
    %Directions = [[1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0],
    %              [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0],
    %              [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0],
    %              [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0],
    %              [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0],
    %              [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0],
    %              [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0],
    %              [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0],
    %              [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0], [1, 1, 0]],
    test_write_start('sample multiple lines gradient - brightness'),
    seq_img(Imgseq, 0, IMG1),
    clone_img(IMG1, IMG2),
    test_multiple_sample_line_L_grad(Imgseq, Centers, Directions, IMG2),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),
    test_write_done.

test_multiple_sample_line_L_grad(_, [], [], _):-
    !.
test_multiple_sample_line_L_grad(Imgseq, [C | Cs], [D | Ds], IMG):-
    sample_line_L_grad(Imgseq, C, D, Pts, G),
    print(Pts), nl,
    print(G), nl,
    items_key_geq_T(Pts, G, 2, HL), % highlight
    items_key_less_T(Pts, G, -1, SHD), % shadow
    draw_points_2d(IMG, HL, red),
    draw_points_2d(IMG, SHD, blue),
    test_multiple_sample_line_L_grad(Imgseq, Cs, Ds, IMG).


test_rand_sample_line_L_grad(Imgseq, N):-
    test_write_start('randomly sample multiple lines gradient - brightness'),
    seq_img(Imgseq, 0, IMG1),
    clone_img(IMG1, IMG2),
    test_rand_sample_line_L_grad(Imgseq, IMG2, N),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),    
    test_write_done.
    
test_rand_sample_line_L_grad(_, _, 0):-
    !.
test_rand_sample_line_L_grad(Imgseq, IMG, N):-
    random_between(0, 639, X), random_between(0, 359, Y),
    random_between(-10, 10, XX), random_between(0, 20, YY), % YY > 0
    C = [X, Y, 0],
    D = [XX, YY, 0],
    sample_line_L_grad(Imgseq, C, D, Pts, G),
    print(Pts), nl,
    print(G), nl,
    items_key_geq_T(Pts, G, 2, Pos), % grad+
    items_key_less_T(Pts, G, -1, Neg), % grad-
    draw_points_2d(IMG, Pos, red),
    draw_points_2d(IMG, Neg, blue),
    N1 is N - 1,
    test_rand_sample_line_L_grad(Imgseq, IMG, N1).

test_deduce_light_source(Imgseq, N, Dir):-
    test_deduce_light_source(Imgseq, N, Dir1, [0, 0, 0]),
    %Dir1 = [X1, Y1, 0],
    %X is X1/N, Y is Y1/N,
    Dir = Dir1.
    %Dir = [X, Y, 0].
test_deduce_light_source(_, 0, Dir, Dir):-
    !.
test_deduce_light_source(Imgseq, N, Dir, Sum):-
    N > 0,
    N1 is N - 1,
    write(N), nl,
    sample_light_source_dir(Imgseq, 0, _, D),
    D = [X, Y, 0],
    X1 is X/sqrt(X**2 + Y**2),
    Y1 is Y/sqrt(X**2 + Y**2),
    Sum = [XX, YY, 0],
    XX1 is X1 + XX, YY1 is Y1 + YY,
    % normalize the direction
    XX2 is XX1/sqrt(XX1**2 + YY1**2),
    YY2 is YY1/sqrt(XX1**2 + YY1**2),
    test_deduce_light_source(Imgseq, N1, Dir, [XX2, YY2, 0]).


test_abduce_light_source(Imgseq, Frame, Sources):-
    abduce(ab_light_source(Imgseq, Frame, 0, [], Sources)).

test_ellipse(Imgseq):-
    test_write_start("ellipse definition"),
    ellipse(Imgseq, [[340, 143, 0], [35, 17, 100]], 2, 0.7, Pos, PTS),
    %ellipse(Imgseq, [[323, 143, 0], [35, 17, 100]], 2, 0.6, Pos, PTS),
    seq_img(Imgseq, 0, IMG1),
    clone_img(IMG1, IMG2),
    draw_points_2d(IMG2, PTS, blue),    
    draw_points_2d(IMG2, Pos, red),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),
    test_write_done.

test_scharr(Imgseq, Point):-
    test_write_start("Schurr gradient calculator"),
    sample_point_scharr(Imgseq, Point, G),
    write("Grad: "), write(G), nl,
    seq_img(Imgseq, 0, IMG1),
    clone_img(IMG1, IMG2),
    draw_points_2d(IMG2, [Point], blue),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),
    test_write_done.

test_line_scharr(Imgseq, Point, Dir, Thresh):-
    test_write_start("Schurr gradient calculator"),
    %sample_line_scharr(Imgseq, Point, Dir, Pts, Grads),
    size_3d(Imgseq, W, H, D),
    line_points(Point, Dir, [W, H, D], Pts),
    line_pts_scharr_geq_T(Imgseq, Point, Dir, Thresh, Pos),
    %items_key_geq_T(Pts, Grads, Thresh, Pos),
    seq_img(Imgseq, 0, IMG1),
    clone_img(IMG1, IMG2),
    draw_points_2d(IMG2, Pts, blue),    
    draw_points_2d(IMG2, Pos, red),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),
    test_write_done.

test_rand_sample_lines_scharrs(Imgseq, N):-
    test_write_start('randomly sample multiple lines for points with large scharr gradient'),
    seq_img(Imgseq, 0, IMG1),
    clone_img(IMG1, IMG2),
    test_rand_sample_line_scharr(Imgseq, Pos, N, []),
    % most probable edge points
    pts_scharr(Imgseq, Pos, Grads),
    mapsort(Grads, Pos, _, S_Pos),
    lastN(500, S_Pos, PPos),
    firstN(3000, S_Pos, NPos),
    draw_points_2d(IMG2, Pos, blue),
    draw_points_2d(IMG2, PPos, red),
    draw_points_2d(IMG2, NPos, green),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),    
    test_write_done.
    
test_rand_sample_line_scharr(_, Re, 0, Re):-
    !.
test_rand_sample_line_scharr(Imgseq, Re, N, Tmp):-
    random_between(0, 639, X), random_between(0, 359, Y),
    random_between(-10, 10, XX), random_between(0, 20, YY), % YY > 0
    C = [X, Y, 0],
    ((XX == 0, YY == 0) ->
         (Dir = [1, 1, 0], !);
     (Dir = [XX, YY, 0], !)),
    %size_3d(Imgseq, W, H, D),
    %line_points(C, Dir, [W, H, D], Pts),
    line_pts_scharr_geq_T(Imgseq, C, Dir, 2, Pos),
    append(Tmp, Pos, Tmp1),
    %sample_line_L_grad(Imgseq, C, D, Pts, G),
    %print(Pts), nl,
    %draw_points_2d(IMG, Pts, blue),
    %draw_points_2d(IMG, Pos, red),
    N1 is N - 1,
    test_rand_sample_line_scharr(Imgseq, Re, N1, Tmp1).

test_compare_hist(Imgseq):-
    test_write_start("test compare histograms"),
    size_3d(Imgseq, W, H, D),
    line_pts_scharr_geq_T(Imgseq, [335, 133, 0], [1, 1, 0], 2, Pos),
    Pos = [P1, P2, P3, _, P5, P6 | _],
    line_seg_points(P1, P2, [W, H, D], Pts1),
    line_seg_points(P2, P3, [W, H, D], Pts2),
    line_seg_points(P5, P6, [W, H, D], Pts3),
    writeln(Pts1),
    writeln(Pts2),
    compare_hist(Pts1, Pts2, Dist1),
    compare_hist(Pts1, Pts3, Dist2),
    write("histogram KL divergence ([P1, P2], [P2, P3]): "), write(Dist1), nl,
    write("histogram KL divergence ([P1, P2], [P5, P6]): "), write(Dist2), nl,
    test_write_done.

test_points_hist(Imgseq):-
    test_write_start("test points histograms"),
    size_3d(Imgseq, W, H, D),
    line_pts_scharr_geq_T(Imgseq, [335, 133, 0], [1, 1, 0], 2, Pos),
    Pos = [P1, P2, P3, _, P5, P6 | _],
    line_seg_points(P1, P2, [W, H, D], Pts1),
    line_seg_points(P2, P3, [W, H, D], Pts2),
    line_seg_points(P5, P6, [W, H, D], Pts3),
    points_color_hist(Imgseq, Pts1, H1),
    points_color_hist(Imgseq, Pts2, H2),
    points_color_hist(Imgseq, Pts3, H3),
    print_list(H1),
    hist_diff(H1, H2, Dist1),
    hist_diff(H1, H3, Dist2),
    write("histogram KL divergence (H1, H2): "), write(Dist1), nl,
    write("histogram KL divergence (H1, H3): "), write(Dist2), nl,
    test_write_done.

test_sample_line_seg_hists(Imgseq):-
    test_write_start("test sample line and get Seg-Hist pairs"),
    sample_line_hists(Imgseq, [[335, 133, 0], [1, 1, 0]], 2, SHs),
    print_list(SHs).

% randomly sample many lines and get there histograms, then cluster them
test_cluster_rand_segs(Imgseq):-
    test_write_start("test randomly sample lines and do clustering"),
    size_2d(Imgseq, W, H),
    %rand_sample_lines(500, Lines),
    sample_horizon_lines(2000, [W, H], 50, Lines1),
    sample_vertical_lines(2000, [W, H], 100, Lines2),
    append(Lines1, Lines2, Lines),
    write('sampling finished.'), nl,
    sample_lines_hists(Imgseq, Lines, 2, SHs),
    write('computing histograms finished.'), nl,
    cluster_seg_hist_pairs(SHs, 2, SHs_Sign, Cents),
    write('clustering finished.'), nl,
    write('Centroids:'), nl,
    print_list_ln(Cents),
    % print_list_ln(SHs_Sign),
    seq_img(Imgseq, 2000, IMG1),
    clone_img(IMG1, IMG2),    
    test_display_SHs(IMG2, SHs_Sign),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),
    test_write_done.

test_display_SHs(_, []):-
    !.
test_display_SHs(Img, [[Start, End]-_-0 | SHs]):-
    draw_line_seg_2d(Img, Start, End, r),
    test_display_SHs(Img, SHs), !.
test_display_SHs(Img, [[Start, End]-_-1 | SHs]):-
    draw_line_seg_2d(Img, Start, End, g),
    test_display_SHs(Img, SHs), !.
test_display_SHs(Img, [[Start, End]-_-2 | SHs]):-
    draw_line_seg_2d(Img, Start, End, b),
    test_display_SHs(Img, SHs), !.
test_display_SHs(Img, [[Start, End]-_-3 | SHs]):-
    draw_line_seg_2d(Img, Start, End, y),
    test_display_SHs(Img, SHs), !.
test_display_SHs(Img, [[Start, End]-_-4 | SHs]):-
    draw_line_seg_2d(Img, Start, End, w),
    test_display_SHs(Img, SHs), !.
test_display_SHs(Img, [[Start, End]-0 | SHs]):-
    draw_line_seg_2d(Img, Start, End, r),
    test_display_SHs(Img, SHs), !.
test_display_SHs(Img, [[Start, End]-1 | SHs]):-
    draw_line_seg_2d(Img, Start, End, g),
    test_display_SHs(Img, SHs), !.
test_display_SHs(Img, [[Start, End]-2 | SHs]):-
    draw_line_seg_2d(Img, Start, End, b),
    test_display_SHs(Img, SHs), !.
test_display_SHs(Img, [[Start, End]-3 | SHs]):-
    draw_line_seg_2d(Img, Start, End, y),
    test_display_SHs(Img, SHs), !.
test_display_SHs(Img, [[Start, End]-4 | SHs]):-
    draw_line_seg_2d(Img, Start, End, w),
    test_display_SHs(Img, SHs), !.

% test sample cubes
test_sample_cube_var_hist(Imgseq):-
    test_write_start("test cube sampling"),
    %sample_cube_var(Imgseq, [353, 133, 0], [3, 3, 0], Var),
    sample_cube_var(Imgseq, [100, 100, 0], [3, 3, 0], Var),
    write("Var: "), write(Var), nl,
    %sample_cube_hist(Imgseq, [353, 133, 0], [3, 3, 0], Hist),
    sample_cube_hist(Imgseq, [100, 100, 0], [3, 3, 0], Hist),
    write("Hist: "), print_list(Hist), nl,
    test_write_done.

% test draw cubes
test_draw_cubes(Imgseq):-
    test_write_start("test draw cubes"),
    clone_seq(Imgseq, Seq),
    seq_img(Seq, 0, Img),
    draw_rect_2d(Img, [100, 150, 0], [37, 18, 110], gree),
    showimg_win(Img, '2d'),
    draw_rect(Seq, [353, 143, 0], [37, 18, 110], red),    
    showseq_win(Seq, '3d'),
    release_imgseq(Seq),
    test_write_done.

test_color_bg_nonbg(Imgseq, [BG, NonBG]):-
    test_write_start('test color_bg_nonbg'),
    color_bg_nonbg(Imgseq, 0, [BG, NonBG], NBGs),
    write('BG centroid: '), nl, print(BG), nl,
    write('Non-BG centroid: '), nl, print(NonBG), nl,
    seq_img(Imgseq, 0, Img),
    clone_img(Img, Img1),
    longest_seg(NBGs, Longest),
    %draw_line_segs_2d(Img1, NBGs, r),
    draw_line_segs_2d(Img1, [Longest], r),
    showimg_win(Img1, 'debug'),
    release_img(Img1),
    test_write_done.

test_sample_line_get_seg_classes(Imgseq, Point, Dir):-
    test_write_start('test sample_line_get_seg_classes'),
    test_color_bg_nonbg(Imgseq, Cents),
    sample_line_get_seg_classes(Imgseq, Cents, Point, Dir, Seg_Class),
    write('seg class: '), nl,
    print_list_ln(Seg_Class),
    seq_img(Imgseq, 0, Img),
    clone_img(Img, Img1),
    test_display_SHs(Img1, Seg_Class),
    showimg_win(Img1, 'debug'),
    release_img(Img1),
    test_write_done.

test_seg_seg_dist(I):-
    test_write_start('test seg_seg_distance.'),
    (I == 1 ->
         % ex 1~~9.7307189345304538e-10
         (P0 = [-1.0264718499965966, 9.6163341007195407e-7, 0.0],
          P1 = [0.91950808032415809, -1.0094441192690283e-6, 0.0],
          Q0 = [-1.0629447383806110, 9.2709540082141753e-7, 0.0],
          Q1 = [1.0811583868227901, -1.0670017179567367e-6, 0.0], !);
     (I == 2 ->
          % ex 2~~1.1575046138574101e-7
          (P0 = [-1.0896217473782599, 9.7236145595088601e-7, 0.0],
           P1 = [0.91220578597858548, -9.4369829432107506e-7, 0.0],
           Q0 = [-0.90010447502136237, 9.0671446351334441e-7, 0.0],
           Q1 = [1.0730877178721130, -9.8185787633992740e-7, 0.0], !);
      (I == 3 ->
           % ex 3~0.98292397116488739
           (P0 = [0.77998990099877119, 0.61192502360790968, -0.22703111823648214],
            P1 = [0.53215344529598951, 0.85724585503339767, -0.10102437809109688],
            Q0 = [-0.21277333982288837, 0.35091548087075353, -0.49557160679250956],
            Q1 = [0.11881479667499661, 0.022494725417345762, -0.66426620958372951],
            !);
       % ex 4~~2.7122314947662727e-17
       (Delta is 0.25*1e-4,
        Epsilon is sqrt(Delta),
        Phi is 1e-5,
        P0 = [0,0,0],
        P1 = [1,0,0],
        QQ0 is Phi + Delta,
        QQ1 is Phi - Delta,
        EE0 is -Epsilon,
        Q0 = [EE0, QQ0, 0],
        Q1 = [Epsilon, QQ1, 0], !)
      )
     )
    ),
    seg_seg_distance([P0, P1], [Q0, Q1], Dist, Closest),
    write('Dist: '), write(Dist), nl,
    write('Closest Points: '), nl, print_list_ln(Closest),
    test_write_done.

test_object_discover(Imgseq, Frm):-
    test_write_start('test object discovering'),
    color_bg_nonbg(Imgseq, Frm, Cents, NonBGSegs),
    discover_object(Imgseq, Frm, Cents, NonBGSegs, Elpses),
    print_list_ln(Elpses),
    seq_img(Imgseq, Frm, IMG1),
    clone_img(IMG1, IMG2),
    test_draw_elpses(IMG2, Elpses),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),
    test_write_done.

test_draw_elpses(_, []):-
    !.
test_draw_elpses(Img, [elps(Cen, Para, C) | Elpses]):-
    size_2d(Img, W, H),
    ellipse_points(Cen, Para, [W, H, 1000000], Pts),
    ((C == 0, Col = r);
     (C == 1, Col = g);
     (C == 2, Col = b);
     (C == 3, Col = y);
     (C == 4, Col = k)),
    !,
    draw_points_2d(Img, Pts, Col),
    test_draw_elpses(Img, Elpses).

% test gradient points
test_extrema(Imgseq, Point, Dir):-
    test_write_start('test gradient zeropoint (extrema of brightness)'),
    % get points and brightness    
    sample_line_L_grad(Imgseq, Point, Dir, Pts, G),
    %grad(G1, G), % second order derivative
    print(Pts), nl,
    %print(G1), nl,
    print(G), nl,
    items_key_geq_T(Pts, G, 2, HL), % highlight
    items_key_less_T(Pts, G, -1, SHD), % shadow
    pairs_keys_values(Pair, G, Pts),
    zero_item(Pair, -1, ExtPts),
    Point = [_, _, Frame],
    seq_img(Imgseq, Frame, IMG1),
    clone_img(IMG1, IMG2),
    draw_points_2d(IMG2, HL, red),
    draw_points_2d(IMG2, SHD, blue),    
    draw_points_2d(IMG2, ExtPts, green),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),    
    test_write_done.

%test_extremas(Imgseq):-
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HERE GOES MAIN TEST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_main:-
    test_load_imgseq(Imgseq),
    test_show2imgs(Imgseq),
    test_draw_elps(Imgseq, [[353, 143, 0], [37, 18, 110]], red),
    test_fit_elps(Imgseq, [353, 143, 0], [37, 18, 20], red),
    test_draw_line_2d(Imgseq, [100, 100, 0], [2, 7, 0], red),
    test_draw_line_(Imgseq, [100, 100, 0], [2, 7, 0], red),
    test_ellipse(Imgseq),
    test_fit_circle(Imgseq, [100, 100, 30], blue),
    showseq_win(Imgseq, debug),
    %test_sample_line_L_grad(Imgseq, [351, 147, 2000], [1, 1, 0]),
    %test_sample_line_L_grad(Imgseq, [120, 0, 2000], [1, 1, 0]),
    %test_extrema(Imgseq, [351, 147, 0], [1, 1, 0]),
    %test_sample_parallel_lines_L_grad(Imgseq, 0, [1,1,0]),
    test_line_scharr(Imgseq, [353, 133, 0], [1, 1, 0], 2),
    test_line_scharr(Imgseq, [335, 133, 0], [1, 1, 0], 2),
    test_compare_hist(Imgseq),
    test_points_hist(Imgseq),
    test_rand_sample_lines_scharrs(Imgseq, 1000),
    test_sample_line_seg_hists(Imgseq),
    %test_cluster_rand_segs(Imgseq).
    %test_sample_cube_var_hist(Imgseq),
    %test_draw_cubes(Imgseq),
    %test_color_bg_nonbg(Imgseq, _),
    %test_sample_line_get_seg_classes(Imgseq, [353, 133, 0], [1, 2, 0]),
    test_object_discover(Imgseq, 0),
    %debug(Imgseq),
    test_rel_s(Imgseq).    
    %learn(pairing).
    

% test library without image processing
test_lib(Param):-
    test_seg_seg_dist(Param).

test([], []):-!.
test([S | Ss], [S | Ds]):-
    S < 0,
    test(Ss, Ds), !.
test([_ | Ss], Ds):-
    test(Ss, Ds), !.

%%%%%%%%%%%%%%%%%%%%%%%
% PREDICATES TO DEBUG
%%%%%%%%%%%%%%%%%%%%%%%
test_debug:-
    test_load_imgseq(Imgseq),
    A = [elps([341,145,0],[14,40,95],1),elps([479,326,0],[25,45,115],1),elps([352,151,0],[17,29,91],1),elps([360,151,0],[16,24,77],1),elps([343,147,0],[11,42,88],1),elps([339,129,0],[8,23,80],2),elps([339,129,0],[8,25,80],2),elps([339,143,0],[20,29,76],2),elps([454,342,0],[26,62,64],2)],
    seq_img(Imgseq, 0, IMG1),
    clone_img(IMG1, IMG2),
    test_draw_elpses(IMG2, A),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),
    test_rel_s(Imgseq).
