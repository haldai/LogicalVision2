/* Test module
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

:- ['../io/plio.pl'],
   ['../sampling/plsampling.pl'],
   ['../drawing/pldraw.pl'],
   ['../utils/utils.pl'].

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

% test draw line (on image sequence)
test_draw_line(IMGSEQ, Point, Dir, Color):-
    test_write_start('test draw line (3d)'),
    clone_seq(IMGSEQ, SEQ1),
    draw_line(SEQ1, Point, Dir, Color),
    showseq_win(SEQ1, 'debug'),
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
test_draw_elps(IMGSEQ, Center, [A, B, ALPHA], COLOR):-
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

% test utilities
test_write_start(Name):-
    write('[TEST] '), write(Name), write('.'), nl.
test_write_done:-
    write('[DONE]'), nl,
    write('================'), nl.
