/* Test module - 3
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

:- ensure_loaded(['../io/plio.pl',
                  '../sampling/plsampling.pl',
                  '../drawing/pldraw.pl',
                  '../stats/plstats.pl',
                  '../abduce/plabduce.pl',
                  '../abduce/bk_light.pl',
                  '../abduce/bk_ellipse.pl',
                  '../abduce/bk_polygon.pl',
                  '../abduce/bk_football.pl',
                  '../stats/ball_region.pl',
                  '../sampling/plregion.pl',
                  '../sampling/plpoints.pl',
                  '../utils/utils.pl',
                  '../learning/pllearning.pl',
                  'test2.pl',
                  'test.pl']).

test_load_img_3(A):-
    test_write_start('load image'),
    %load_img('../../data/Protist0.png', A),
    load_img('../../data/late.png', A),
    size_2d(A, X, Y),
    write('W x H: '),
    write(X), write(' x '), write(Y), nl,
    test_write_done.

test_fit_elps_2d(Img, Center, [A, B, Alpha], Color):-
    test_write_start('fit ellipse'),
    clone_img(Img, Img2),
    size_2d(Img, W, H),
    ellipse_points_2d(Center, [A, B, Alpha], [W, H], Pts),
    index_select([1, 30, 50, 70, 90, 110, 130, 150, 170, 190], Pts, Pts2),
    %Pts2 = Pts,
    fit_elps_2d(Pts2, Cen2, Para2),
    write('fit parameters: '), write(Cen2), write(', '), write(Para2), nl,
    ellipse_points_2d(Cen2, Para2, [W, H], Pts3),
    draw_points_2d(Img2, Pts3, Color),
    draw_points_2d(Img2, Pts2, green),
    showimg_win(Img2, 'debug'),
    release_img(Img2),
    test_write_done.

test_line_scharr_2d(Img, Point, Dir, Thresh):-
    test_write_start("Scharr gradient calculator"),
    %sample_line_scharr(Imgseq, Point, Dir, Pts, Grads),
    size_2d(Img, W, H),
    line_points_2d(Point, Dir, [W, H], Pts),
    line_pts_scharr_geq_T_2d(Img, Point, Dir, Thresh, Pos),
    %items_key_geq_T(Pts, Grads, Thresh, Pos),
    clone_img(Img, Img2),
    draw_points_2d(Img2, Pts, blue),    
    draw_points_2d(Img2, Pos, red),
    showimg_win(Img2, 'debug'),
    release_img(Img2),
    test_write_done.

test_train_stat_pts(Img):-
    test_write_start('train stat model for points'),
    writeln('Gen training data: '),
    time(gen_pts_train_data('../../data/Protist0.png',
                            '../../data/Protist0_fg.bmp',
                            200, Data_Label_1)),
    %print_list_ln(Data_Label),
    subsample(1.0, 0.25, Data_Label_1, Data_Label),
    findall(N, member(N-0, Data_Label), Ns), length(Ns, Len_N),
    findall(P, member(P-1, Data_Label), Ps), length(Ps, Len_P),
    write('Neg num: '), writeln(Len_N),
    write('Pos num: '), writeln(Len_P),
    %train_adaboost(Data_Label, Model),
    train_svm(Data_Label, Model),
    writeln('Gen test data: '),
    gen_pts_train_data('../../data/Protist0.png',
                       '../../data/Protist0_fg.bmp',
                       10, Data_Label_2),
    pairs_keys_values(Data_Label_2, Test_Data, Test_Label),
    %time(predict_adaboost(Model, Test_Data, Pred_Label)),
    time(predict_svm(Model, Test_Data, Pred_Label)),
    %print_list(Pred_Label),
    eval_bin_acc(Test_Label, Pred_Label, Acc),
    eval_bin_precision(Test_Label, Pred_Label, Pre),
    eval_bin_recall(Test_Label, Pred_Label, Rec),
    write('Accuracy: '), writeln(Acc),
    write('Precision: '), writeln(Pre),
    write('Recall: '), writeln(Rec),
    writeln('Test on an line: '),
    size_2d(Img, W, H),
    time((
                %sample_line_color_L_hist_2d(Img, [353, 133], [1, 1], Line_Pts, Line_Hist),
                rand_lines_2d([W, H], 200, Lines),
                sample_lines_color_L_hist_2d(Img, Lines, Line_Pts, Line_Hist),
                %sample_line_color_hist_2d(Img, [353, 133], [1, 1], Line_Pts, Line_Hist),
                predict_svm(Model, Line_Hist, Line_Label))
        ),
    %predict_adaboost(Model, Line_Hist, Line_Label),
    pairs_keys_values(Pts_Labels, Line_Pts, Line_Label),
    clone_img(Img, Img2),
    draw_points_with_label_2d(Img2, Pts_Labels),
    showimg_win(Img2, 'test_stat_model'),
    release_img(Img2),
    release_model_svm(Model),
    %release_model_ada(Model),
    test_write_done.

test_L_hist_change_2d(Img, Pt, Dir):-
    test_write_start('test color L histogram changing'),
    sample_line_color_L_hist_change_2d(Img, Pt, Dir, _, Hist_Chg),
    print_list(Hist_Chg),
    test_write_done.

test_main_3:-
    test_load_img_3(Img),
    %test_fit_elps_2d(Img, [353, 143], [37, 18, 20], red),
    %test_line_scharr_2d(Img, [353, 133], [1, 1], 5),
    %test_train_stat_pts(Img),
    test_L_hist_change_2d(Img, [353, 133], [1, 1]),
    test_rel_img(Img).
