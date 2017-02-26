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
/* Test module
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
                  '../utils/utils.pl',
                  '../learning/pllearning.pl',
                  'test.pl']).

% test load image
test_load_img(A):-
    test_write_start('load image'),
    %load_img('../../data/MobileRobotAndBall1/balls/65.jpg', A),
    %load_img('../../data/MobileRobotAndBall1/balls/1475.jpg', A),
    load_img('../../data/MobileRobotAndBall1/raw_images/1485.jpg', A),
    %load_img('../../data/MobileRobotAndBall1/raw_images/65.jpg', A),
    size_2d(A, X, Y),
    write('W x H: '),
    write(X), write(' x '), write(Y), nl,
    test_write_done.

test_rel_img(A):-
    test_write_start('release image'),
    release_img(A),
    test_write_done.

test_showimg(A):-
    test_write_start('show image'),
    showimg_win(A, 'debug'),
    test_write_done.

test_subimg(A, [X, Y, RX, RY]):-
    test_write_start('subimage'),
    subimg_2d(A, [X, Y, RX, RY], B),
    showimg_win(B, 'subimage'),
    release_img(B),
    test_write_done.

test_resize_img(A, [X, Y]):-
    test_write_start('resize image'),
    resize_img_2d(A, [X, Y], B),
    showimg_win(B, 'resized image'),
    release_img(B),
    test_write_done.

test_edge_seg(A, [P1, P2]):-
    test_write_start('evaluate edge segment'),
    write([P1, P2]),
    (edge_seg(A, [P1, P2]) ->
         (write(' is an edge.'), nl, !);
     (write(' is NOT an edge.'), nl, !)),
    test_write_done.

test_sample_line_2d(Img):-
    test_write_start('sample line scharr to get edge points'),
    sample_edge_pts_2d(Img, [[30, 30], [1, 1]], Pts),
    write(Pts),
    clone_img(Img, Img1),
    draw_points_2d(Img1, Pts, red),
    showimg_win(Img1, 'debug'),
    release_img(Img1),    
    test_write_done.

test_edge_conj(Img):-
    test_write_start('edge conjecturing'),
    size_2d(Img, W, H),
    rand_sample_2d_lines(0, [W, H], 50, Ls),
    sample_edge_pts_2d(Img, Ls, EPs, []),
    new_conjs(EPs, EPs, Conj),
    conjecture_edge_2d(Img, EPs, Conj, Edges),
    %print_list(Edges),
    clone_img(Img, Img1),
    draw_line_segs_2d(Img1, Edges, red),
    showimg_win(Img1, 'debug'),
    release_img(Img1),    
    test_write_done.

test_sample_color_change_pts(Img, [Start, Direct]):-
    test_write_start('sample color changing points'),
    sample_line_color_2d(Img, Start, Direct, Points, LABs),
    colors(LABs, Colors),
    pairs_keys_values(Pairs, Colors, LABs),
    print_list(Pairs),
    sample_line_color_change_2d(Img, [Start, Direct], green, not(green), Chgd0),
    sample_line_color_change_2d(Img, [Start, Direct], white, black, Chgd1),
    sample_line_color_change_2d(Img, [Start, Direct], black, white, Chgd2),
    sample_line_color_change_2d(Img, [Start, Direct], not(green), green, Chgd3),
    %color_change_point(green, not(green), Points, LABs, Chgd0),
    %color_change_point(white, black, Points, LABs, Chgd1),
    %color_change_point(black, white, Points, LABs, Chgd2),
    %color_change_point(not(green), green, Points, LABs, Chgd3),
    append(Chgd0, Chgd3, Chgd),
    clone_img(Img, Img1),
    draw_points_2d(Img1, Points, g),
    draw_points_2d(Img1, Chgd1, b),
    draw_points_2d(Img1, Chgd2, y),
    draw_points_2d(Img1, Chgd, r),    
    showimg_win(Img1, 'debug'),
    save_img(Img1, '../../tmp/tmp.png'),
    release_img(Img1),
    test_write_done.

test_sample_GNG_pts(Img, [Start, Direct]):-
    test_write_start('sample green/non-green points'),
    sample_line_GNG_points_2d(Img, [Start, Direct], GNGs),
    pts2segs(GNGs, GNG_Segs),
    print_list(GNG_Segs),    
    findall(BW, (member(BW, GNG_Segs), bw_seg_2d(Img, BW)), BW_Segs),
    print_list(BW_Segs),
    clone_img(Img, Img1),
    size_2d(Img, W, H),
    line_points_2d(Start, Direct, [W, H], Points),
    draw_points_2d(Img1, Points, g),
    draw_points_2d(Img1, GNGs, r),
    draw_line_segs_2d(Img1, BW_Segs, yellow),
    showimg_win(Img1, 'debug'),
    release_img(Img1),
    test_write_done.

test_fit_circle(Img, [X, Y, R]):-
    test_write_start('fit circle'),
    size_2d(Img, W, H),
    circle_points([X, Y, 0, R], [W, H, 1], PTS),
    %index_select([1, 30, 50, 70, 90, 110, 130, 150, 170, 190], PTS, PTS2),
    index_select([30, 50, 190], PTS, PTS2),
    %PTS2 = PTS,
    fit_circle(PTS2, Para2),
    write('fit parameters: '), write(Para2), nl,
    circle_points(Para2, [W, H, 1], PTS3),
    clone_img(Img, IMG2),
    draw_points_2d(IMG2, PTS3, blue),
    draw_points_2d(IMG2, PTS2, red),
    showimg_win(IMG2, 'debug'),
    release_img(IMG2),
    test_write_done.

test_spiral(Img):-
    test_write_start('test get spiral points.'),
    consult('../../data/MobileRobotAndBall1/football.pl'),
    football('/home/daiwz/Myprojects/LogicalVision2/data/MobileRobotAndBall1/raw_images/65.jpg', Box),
    %football('/home/daiwz/Myprojects/LogicalVision2/data/MobileRobotAndBall1/raw_images/1485.jpg', Box),
    size_2d(Img, W, H),
    time(rand_spiral_sampling([W, H], [0, 0.5], 1, Spiral_Segs)),
    % print_list_ln(Spiral_Segs),
    append(Spiral_Segs, Spiral_Pts),
    time(get_GNG_seg_lists(Img, Spiral_Segs, GNGSegs)),
    time(gen_ball_instances(Box, GNGSegs, Pos, Neg)),
    time(get_colors_list_2d(Img, Pos, P_List)),
    time(get_colors_list_2d(Img, Neg, N_List)),
    %print_list(QQQQ),
    % writeln("POS:"),
    % print_list_ln(P_List),
    % writeln("NEG:"),
    % print_list_ln(N_List),
    %A = [[257,492],[253,484],[250,475],[247,467],[245,458],[243,450]],
    %append(GNGSegs, GNGPts),
    %clone_img(Img, Img2),
    %draw_points_2d(Img2, Spiral_Pts, red),
    %draw_points_2d(Img2, GNGPts, blue),
    %%draw_points_2d(Img2, A, blue),
    %showimg_win(Img2, 'debug'),
    %save_img(Img2, '../../tmp/spiral_segs_2.png'),
    %release_img(Img2),
    unload_file('../../data/MobileRobotAndBall1/football.pl'),
    test_write_done.

test_gen_ball_train:-
    test_write_start('test generating ball training instances'),
    gen_ball_train,
    test_write_done.

test_gen_ball_train_2:-
    test_write_start('test generating ball training instances 2'),
    gen_ball_train_2,
    test_write_done.

test_spiral_region(Img):-
    test_write_start('test spiral regions sampling.'),
    size_2d(Img, W, H),
    % samples the regions/lines
    time(spiral_regions_2d([300, 500], [W, H], [1, 1], [1.5, 1], Regions)),
    print_list_ln(Regions),
    % 
    clone_img(Img, Img2),
    draw_squares_2d(Img2, Regions, red),
    showimg_win(Img2, 'debug'),
    test_write_done.

test_color_hist(Img):-
    test_write_start('test stats module, color histogram'),
    color_hist_rect_2d(Img, [300, 300], [20, 20], H1),
    color_hist_rect_2d(Img, [253, 464], [16, 26], H2),
    print_list(H1),
    print_list(H2),
    test_write_done.

test_ball_adaboost_1:-
    test_write_start('test adaboost model for ball recon'),
    % labeled bounding boxes
    consult('../../data/MobileRobotAndBall1/football.pl'),
    %Path = '/home/daiwz/Myprojects/LogicalVision2/data/MobileRobotAndBall1/raw_images/1485.jpg',
    Path = '/home/daiwz/Myprojects/LogicalVision2/data/MobileRobotAndBall1/raw_images/65.jpg',
    writeln('Training: '),
    gen_ball_region_data(Path, 50, 0.5, Reg_Train, Data_Labels),
    pairs_keys_values(Data_Labels, _, L),
    pairs_keys_values(PP, L, Reg_Train),
    findall(TrPos, member(1-TrPos, PP), PPos),
    load_img(Path, Img0),
    draw_squares_2d(Img0, PPos, red),
    showimg_win(Img0, 'Train Pos'),
    release_img(Img0),
    %print_list_ln(Data_Labels),
    train_adaboost(Data_Labels, Model),
    write("Model: "), writeln(Model),
    writeln('\nTesting: '),
    gen_ball_region_data(Path, 5, 1.0, Regions, Test_Data_Labels),
    pairs_keys_values(Test_Data_Labels, Test_Data, Test_Labels),    
    pairs_keys_values(PTe, Test_Labels, Regions),
    findall(TeP, member(1-TeP, PTe), TePos),
    load_img(Path, Img1),
    draw_squares_2d(Img1, TePos, red),
    showimg_win(Img1, 'Test Pos'),
    release_img(Img1),
    write("Predition: "),
    time(predict_adaboost(Model, Test_Data, Predicted)),
    %print_list(Test_Labels),
    %print_list(Predicted),
    writeln("Evaluation: "),
    eval_bin_acc(Test_Labels, Predicted, Acc),
    eval_bin_precision(Test_Labels, Predicted, Prec),
    eval_bin_recall(Test_Labels, Predicted, Recall),
    write("Prediction accuracy: "), write(Acc), writeln('.'),
    write("Prediction precision: "), write(Prec), writeln('.'),
    write("Prediction recall: "), write(Recall), writeln('.'),
    release_model_ada(Model),
    % have a look on the results
    pairs_keys_values(Pairs, Predicted, Regions),
    findall(PreP, member(1-PreP, Pairs), Pos),
    load_img(Path, Img),
    draw_squares_2d(Img, Pos, red),
    showimg_win(Img, 'Pred Pos'),
    release_img(Img),
    unload_file(['../../data/MobileRobotAndBall1/football.pl']),
    test_write_done.

test_ball_adaboost:-
    test_write_start('test adaboost model for ball recon'),
    writeln('Training: '),
    gen_ball_train_region(50, 0.5, Data_Labels),
    train_adaboost(Data_Labels, Model),
    write("Model: "), writeln(Model),
    writeln('\nTesting: '),
    gen_ball_train_region(5, 1.0, Test_Data_Labels),
    pairs_keys_values(Test_Data_Labels, Test_Data, Test_Labels),
    write("Predition: "),
    time(predict_adaboost(Model, Test_Data, Predicted)),
    writeln("Evaluation: "),
    eval_bin_acc(Test_Labels, Predicted, Acc),
    eval_bin_precision(Test_Labels, Predicted, Prec),
    eval_bin_recall(Test_Labels, Predicted, Recall),
    write("Prediction accuracy: "), write(Acc), writeln('.'),
    write("Prediction precision: "), write(Prec), writeln('.'),
    write("Prediction recall: "), write(Recall), writeln('.'),
    release_model_ada(Model),
    test_write_done.
    
%===========
% test_main
%===========
test_main_2:-
    test_load_img(Img),
    %test_subimg(Img, [100, 100, 50, 300]),
    %test_resize_img(Img, [200, 400]),
    %test_showimg(Img),
    %test_edge_seg(Img, [[19, 26], [27, 26]]),
    %test_edge_seg(Img, [[26, 23], [21, 35]]),
    %test_edge_seg(Img, [[26, 23], [30, 9]]),
    %test_sample_line_2d(Img),
    %test_sample_color_change_pts(Img, [[26, 26], [1, 2.5]]),
    %test_sample_color_change_pts(Img, [[257, 477], [-1, 10]]),
    test_sample_GNG_pts(Img, [[257, 477], [-1, 10]]),
    %test_fit_circle(Img, [100, 100, 30]),
    %test_edge_conj(Img),
    %test_spiral(Img),
    %test_gen_ball_train,
    %test_gen_ball_train_2,
    %test_spiral_region(Img),
    %test_color_hist(Img),
    %test_ball_adaboost_1,
    %test_ball_adaboost,
    test_rel_img(Img).

