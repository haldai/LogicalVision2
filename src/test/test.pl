/* Test module
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

:-ensure_loaded(['../abduce/plabduce.pl',
                 '../abduce/bk_light.pl',
                 '../io/plio.pl',
                 '../sampling/plsampling.pl',
                 '../drawing/pldraw.pl',
                 '../utils/utils.pl']).

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

% sample a line and brightness gradient
test_sample_line_L_grad(Imgseq, Pts, G):-
    test_write_start('sample line gradient - brightness'),
    % get points and brightness
    sample_line_L_grad(Imgseq, [351, 147, 0], [-2, -1, 0], Pts, G),
    print(Pts), nl,
    print(G), nl,
    items_key_geq_T(Pts, G, 2, HL), % highlight
    items_key_less_T(Pts, G, -1, SHD), % shadow
    seq_img(Imgseq, 0, IMG1),
    clone_img(IMG1, IMG2),
    draw_points_2d(IMG2, HL, red),
    draw_points_2d(IMG2, SHD, blue),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),    
    test_write_done.
test_sample_line_L_grad(Imgseq, Pts, G):-
    test_write_start('sample line gradient - brightness'),
    % get points and brightness
    sample_line_L_grad(Imgseq, [351, 147, 0], [-2, -1, 0], Pts, G),
    print(Pts), nl,
    print(G), nl,
    items_key_geq_T(Pts, G, 2, HL), % highlight
    items_key_less_T(Pts, G, -1, SHD), % shadow
    seq_img(Imgseq, 0, IMG1),
    clone_img(IMG1, IMG2),
    draw_points_2d(IMG2, HL, red),
    draw_points_2d(IMG2, SHD, blue),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),    
    test_write_done.

test_sample_line_L_grad(Imgseq, Point, Dir, Pts, G):-
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
    draw_points_2d(IMG2, HL, red),
    draw_points_2d(IMG2, SHD, blue),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),    
    test_write_done.

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

% test utilities
test_write_start(Name):-
    write('[TEST] '), write(Name), write('.'), nl.
test_write_done:-
    write('[DONE]'), nl,
    write('================'), nl.
