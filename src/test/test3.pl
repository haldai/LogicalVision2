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
                  '../abduce/bk_object.pl',
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
    %load_img('../../data/protists/09011.jpg', A),
    %load_img('../../data/protists/01023.jpg', A),
    %load_img('../../data/protists/12023.jpg', A),
    load_img('../../data/protists/02030.jpg', A),
    %load_img('../../data/moons/02001.jpg', A),
    %load_img('../../data/moons/02027.jpg', A),
    %load_img('../../data/moons/02011.jpg', A),
    %load_img('../../data/moons/02021.jpg', A),
    %load_img('../../data/moons/09017.jpg', A),  % too noisy
    %load_img('../../data/moons/10010.jpg', A),
    %load_img('../../data/moons/12016.jpg', A),
    %load_img('../../data/moons/07014.jpg', A),
    %load_img('../../data/moons/07012.jpg', A),    
    %load_img('../../data/moons/11018.jpg', A),
    %load_img('../../data/moons/01020.jpg', A),
    %load_img('../../data/moons/11006.jpg', A),
    %load_img('../../data/Protist_new.jpg', A),
    %load_img('../../data/late.png', A),
    size_2d(A, X, Y),
    write('W x H: '),
    write(X), write(' x '), write(Y), nl,
    test_write_done.

test_fit_elps_2d(Img, Center, [A, B, Alpha], P, Color):-
    test_write_start('fit ellipse'),
    clone_img(Img, Img2),
    size_2d(Img, W, H),
    ellipse_points_2d(Center, [A, B, Alpha], [W, H], Pts),
    index_select1([1, 30, 50, 70, 90, 110, 130, 150, 170, 190], Pts, Pts2),
    %Pts2 = [[214,205], [214,206], [195,228], [186,226], [183,216]],
    print_list(Pts2),
    %Pts2 = Pts,
    fit_elps_2d(Pts2, Cen2, Para2),
    write('fit parameters: '), write(Cen2), write(', '), write(Para2), nl,
    ellipse_points_2d(Cen2, Para2, [W, H], Pts3),
    (point_in_ellipse(P, [Cen2, Para2]) ->
         writeln('In ellipse');
     writeln('Not in ellipse')),
    write('Compute distance & closest point: '),
    time(dist_point_elps_2d(P, [Cen2, Para2], Dist, Closest)),
    write('Distance: '), writeln(Dist),
    draw_points_2d(Img2, Pts3, Color),
    draw_points_2d(Img2, Pts2, green),
    draw_point_2d(Img2, P, blue),
    draw_point_2d(Img2, Closest, yellow),
    showimg_win(Img2, 'debug'),
    release_img(Img2),
    test_write_done.

test_fit_circle_2d(Img, Center, Radius, P, Color):-
    test_write_start('fit circle'),
    clone_img(Img, Img2),
    size_2d(Img, W, H),
    circle_points_2d(Center, Radius, [W, H], Pts),
    index_select1([1, 30, 50], Pts, Pts2),
    %Pts2 = [[214,205], [214,206], [195,228], [186,226], [183,216], [100, 100]],
    print_list(Pts2),
    %Pts2 = Pts,
    fit_circle_2d(Pts2, Cen2, Rad2),
    write('fit parameters: '), write(Cen2), write(', '), write(Rad2), nl,
    circle_points_2d(Cen2, Rad2, [W, H], Pts3),
    (point_in_circle(P, [Cen2, Rad2]) ->
         writeln('In circle');
     writeln('Not in circle')),
    write('Compute distance & closest point: '),
    time(dist_point_circle_2d(P, [Cen2, Rad2], Dist, Closest)),
    write('Distance: '), writeln(Dist),
    draw_points_2d(Img2, Pts3, Color),
    draw_points_2d(Img2, Pts2, green),
    draw_point_2d(Img2, P, blue),
    draw_point_2d(Img2, Closest, yellow),
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
                            100, Data_Label_1)),
    %print_list_ln(Data_Label),
    subsample(1.0, 0.3, Data_Label_1, Data_Label),
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
                rand_lines_2d([W, H], 5, Lines),
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

test_L_hist_change_2d(Img, Pt, Dir, T):-
    test_write_start('test color L histogram changing'),
    %sample_line_color_L_hist_change_2d(Img, Pt, Dir, Pts, Hist_Chg),
    sample_line_color_hist_change_2d(Img, Pt, Dir, Pts, Hist_Chg),
    items_key_geq_T(Pts, Hist_Chg, T, Re),
    print_list(Hist_Chg),    
    clone_img(Img, Img2),
    draw_points_2d(Img2, Pts, blue),
    draw_points_2d(Img2, Re, red),
    showimg_win(Img2, 'hist_change'),
    release_img(Img2),
    test_write_done.

test_draw_perpendicular_lines(Img):-
    test_write_start('test perpendicular lines'),
    size_2d(Img, W, H),
    rand_line_2d([W, H], P, D),
    line_points_2d(P, D, [W, H], Pts),
    nth1(1, Pts, St), last(Pts, Lst),
    perpendicular_bisect_line([St, Lst], P2, D2),
    clone_img(Img, Img2),
    draw_line_2d(Img2, P, D, red),
    draw_line_2d(Img2, P2, D2, blue),
    showimg_win(Img2, 'perpendicular'),
    release_img(Img2),
    test_write_done.

test_draw_turning_lines(Img, Deg):-
    test_write_start('test 2d turning angles'),
    size_2d(Img, W, H),
    rand_line_2d([W, H], P, D),
    line_points_2d(P, D, [W, H], Pts1), last(Pts1, E1),
    turn_degree_2d(D, Deg, D2),
    line_points_2d(P, D2, [W, H], Pts2), last(Pts2, E2),
    clone_img(Img, Img2),
    draw_line_seg_2d(Img2, P, E1, red),
    draw_line_seg_2d(Img2, P, E2, blue),
    showimg_win(Img2, 'turn clockwise'),
    release_img(Img2),
    test_write_done.

test_train_model:-
    test_write_start('test trainning, saving and loading stat model'),
    %train_stat_model_protist(Model1),
    train_stat_model_moon(Model2),
    train_stat_model_crater(Model3),
    %release_model_svm(Model1),
    release_model_svm(Model2),
    release_model_svm(Model3),
    test_write_done.
    
test_sample_obj_protist(Img):-
    test_write_start('test sample object'),
    %train_stat_model(Model),
    load_model_svm('../../tmp/SVM_Protist.model', Model),
    writeln('Abduce object: '),
    time(abduce_object_elps(Img, Model, [], Obj, 0)),
    write("fitted ellipse: "), writeln(Obj),
    clone_img(Img, Img2), Obj = elps(C, P),
    size_2d(Img, W, H),
    ellipse_points_2d(C, P, [W, H], Pts),
    draw_points_2d(Img2, Pts, red),
    showimg_win(Img2, 'fitted_ellipse'),
    release_img(Img2),
    release_model_svm(Model),
    get_largest_contrast_angle(Img, Obj, Ang),
    write("largest contrast angle: "), writeln(Ang),
    writeln(clock_angle(obj1, obj2, Ang)),
    test_write_done.

test_sample_obj_moon(Img):-
    test_write_start('test sample object'),
    %train_stat_model(Model),
    load_model_svm('../../tmp/SVM_Moon.model', Model),
    writeln('Abduce object: '),
    time(abduce_object_circle(Img, Model, [], Obj, 0)),
    write("fitted circle: "), writeln(Obj),
    clone_img(Img, Img2), Obj = circle(C, R),
    size_2d(Img, W, H),
    circle_points_2d(C, R, [W, H], Pts),
    draw_points_2d(Img2, Pts, red),
    showimg_win(Img2, 'fitted_circle'),
    release_img(Img2),
    release_model_svm(Model),
    get_largest_contrast_angle(Img, Obj, Ang),
    write("largest contrast angle: "), writeln(Ang),
    test_write_done.

test_split_ellipse(Img, Cen, Param, Ang):-
    test_write_start('test split ellipse'),
    size_2d(Img, W, H),
    time(split_ellipse(Img, elps(Cen, Param), Ang, Front, Rear)),
    pts_color_L_avg_2d(Img, Front, Bright_F),
    pts_color_L_avg_2d(Img, Rear, Bright_R),
    Diff is Bright_F - Bright_R,
    write('Brightness difference: '), writeln(Diff),
    ellipse_points_2d(Cen, Param, [W, H], Edge),
    clone_img(Img, Img2),
    draw_points_2d(Img2, Front, red),
    draw_points_2d(Img2, Rear, blue),
    draw_points_2d(Img2, Edge, yellow),
    showimg_win(Img2, 'ellipse'),
    release_img(Img2),
    test_write_done.

test_split_circle(Img, Cen, Radius, Ang):-
    test_write_start('test split Radius'),
    size_2d(Img, W, H),
    time(split_circle(Img, circle(Cen, Radius), Ang, Front, Rear)),
    pts_color_L_avg_2d(Img, Front, Bright_F),
    pts_color_L_avg_2d(Img, Rear, Bright_R),
    Diff is Bright_F - Bright_R,
    write('Brightness difference: '), writeln(Diff),
    circle_points_2d(Cen, Radius, [W, H], Edge),
    clone_img(Img, Img2),
    draw_points_2d(Img2, Front, red),
    draw_points_2d(Img2, Rear, blue),
    draw_points_2d(Img2, Edge, yellow),
    showimg_win(Img2, 'ellipse'),
    release_img(Img2),
    test_write_done.

test_main_3:-
    test_load_img_3(Img),
    %test_fit_elps_2d(Img, [353, 143], [37, 18, 20], [393, 143], red),
    %test_fit_elps_2d(Img, [100, 100], [30, 50, 0], [50, 20], red),
    %eval_point_between(Img, hahah, elps([100, 100], [30, 50, -20]), [32,89], [77, 120], 0.5, 5),
    %eval_point_between(Img, hahah, circle([100, 100], 50), [100, 150], [150, 100], 0.9, 5),
    %test_fit_circle_2d(Img, [200, 200], 30, [210, 220], red),
    %test_line_scharr_2d(Img, [500, 500], [2, 1], 5),
    %test_draw_perpendicular_lines(Img),
    %test_draw_turning_lines(Img, -450.798),
    %test_train_stat_pts(Img),
    %test_L_hist_change_2d(Img, [500, 500], [1, -2], 0.05),
    test_train_model,
    %test_sample_obj_protist(Img),
    %test_sample_obj_moon(Img),
    %test_split_ellipse(Img, [100, 100], [30, 50, -45], 11),
    %test_split_circle(Img, [100, 100], 30, 11),
    test_rel_img(Img).
