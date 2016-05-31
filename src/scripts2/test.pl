/* Test module
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

:- load_foreign_library(foreign('../prolog2/cvio.so')),
   load_foreign_library(foreign('../prolog2/cvsampler.so')),
   load_foreign_library(foreign('../prolog2/cvdraw.so')).

:- ['../scripts2/utils.pl'].

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

% test utilities
test_write_start(Name):-
    write('[TEST] '), write(Name), write('.'), nl.
test_write_done:-
    write('[DONE]'), nl,
    write('================'), nl.
