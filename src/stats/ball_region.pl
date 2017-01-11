/* Using statistical model to recognise ball region
 * ================================================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */
:- ensure_loaded(['../stats/plstats.pl',
                  '../utils/geometry.pl']).

box_overlap_ratio(0.5).
subsampling_of_neg_ratio(0.016).

%%%%%%%%%%%%%%%%%%%%%%%%%%
% generate ball region data
%%%%%%%%%%%%%%%%%%%%%%%%%%
% given many images generating data
gen_balls_region_data([], _, _, []):-
    !.
gen_balls_region_data([Path | Paths], Num_samp, SRatio, Data_Labels):-
    gen_ball_region_data(Path, Num_samp, SRatio, D1),
    gen_balls_region_data(Paths, Num_samp, SRatio, D2),
    append(D1, D2, Data_Labels), !.

% given an image, return training data for statistical models
gen_ball_region_data(Path, Num_samp, SRatio, Data_Labels):-
    gen_ball_region_data(Path, Num_samp, SRatio, _, Data_Labels).
gen_ball_region_data(Path, Num_samp, SRatio, SRegions, Data_Labels):-
    football(Path, Box),
    load_img(Path, Img), size_2d(Img, W, H),
    write('Sampling: '),
    time(rand_spiral_regions_2d([W, H], [1, 1],
                                [1.5, 1], Num_samp, Regions)),
    write('Labeling & Subsampling: '),
    time(gen_ball_region_labels(Box, Regions, SRatio, SRegions, Labels)),
    write('Feature extraction: '),
    time(color_hists_squares_2d(Img, SRegions, Hists)),
    pairs_keys_values(Data_Labels, Hists, Labels),
    release_img(Img).

% gen_ball_region_labels(+Bounding_Box, +Regions, +Subsampling_Ratio,
%                 -Out_Regions, -Out_Labels).
% given bounding box and a sampled square region, labeling the regions.
% 0 for negative instance, 1 for positives.
% ++++ withs subsampling of negative examples +++
gen_ball_region_labels(_, [], _, [], []):-
    !.
gen_ball_region_labels(Box, [R | Rs], Sub_Ratio, [R | SRs], [1 | Ls]):-
    R = [[CX, CY], Rad],
    X1 = CX - Rad, X2 = CX + Rad, Y1 = CY - Rad, Y2 = CY + Rad,
    rect_area(Box, S1),
    %rect_area([[X1, Y1], [X2, Y2]], S2),
    rect_intsct_area([[X1, Y1], [X2, Y2]], Box, SI),
    box_overlap_ratio(T),
    (SI/S1 >= T), %; SI/S2 >= T), !,
    gen_ball_region_labels(Box, Rs, Sub_Ratio, SRs, Ls), !.
gen_ball_region_labels(Box, [R | Rs], Sub_Ratio, [R | SRs], [0 | Ls]):-
    random(0.0, 1.0, Rand),
    Rand =< Sub_Ratio,
    gen_ball_region_labels(Box, Rs, Sub_Ratio, SRs, Ls), !.
gen_ball_region_labels(Box, [_ | Rs], Sub_Ratio, SRs, Ls):-
    gen_ball_region_labels(Box, Rs, Sub_Ratio, SRs, Ls), !.


%%%%%%%%%%%%%%%% generate training data from a directory %%%%%%%%%%%%%%%%%%%%
gen_ball_train_region(Num_samp, SRatio, Data_Labels):-
    ['/home/daiwz/Myprojects/LogicalVision2/data/MobileRobotAndBall1/football.pl'],
    findall(D_L,
            gen_ball_region_data(_, Num_samp, SRatio, D_L),
            D_Ls),
    append(D_Ls, Data_Labels),
    unload_file('/home/daiwz/Myprojects/LogicalVision2/data/MobileRobotAndBall1/football.pl').
