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
% background knowledge for abducing objects
%============================================================================

% circles, ellipses, polygons
% confidence level (mid-point evaluation)

min_error_dist_ratio(0.02).
min_error_prob(0.8).
min_error_prob_circle(0.9).

:- ensure_loaded(['../utils/utils.pl']).

%====================
% object abduction
%====================
% ellipses
abduce_object_elps(Img, Model, _, Obj, 40):-
    writeln('Reset!'),
    abduce_object_elps(Img, Model, [], Obj, 0), !.
abduce_object_elps(Img, Model, Edge_Points, Obj, Turn):-
    length(Edge_Points, Num_Pts), Num_Pts =< 1,
    size_2d(Img, W, H),
    rand_line_2d([W, H], Pt, Dir),
    sample_line_color_L_hist_2d(Img, Pt, Dir, Line_Pts, Line_Hist),
    predict_svm(Model, Line_Hist, Line_Label),
    Turn1 is Turn + 1,
    ((positive_segs(Line_Pts, Line_Label, PSegs),
      (random_select(PSegs, Seg))) -> % only use one seg
         (abduce_object_elps(Img, Model, Seg, Obj, Turn1), !);
     (abduce_object_elps(Img, Model, [], Obj, Turn1), !)).

abduce_object_elps(Img, Model, Edge_Points, Obj, Turn):-
    length(Edge_Points, Num_Pts),
    Num_Pts < 10, Num_Pts > 1, randset(2, Num_Pts, Indices),
    index_select1(Indices, Edge_Points, [P1, P2]),
    % randomly choose clockwise or inverse clockwise
    mid_point(P1, P2, P0),
    Turn1 is Turn + 1,
    ((sample_middle_point(Img, Model, P0, P1, P2, P), P \= []) ->
               (abduce_object_elps(Img, Model, [P | Edge_Points], Obj, Turn1), !);
     (list_delete(Edge_Points, [P1, P2], Edge_Points_1),
      abduce_object_elps(Img, Model, Edge_Points_1, Obj, Turn1), !)
    ).

abduce_object_elps(Img, Model, Edge_Points, Obj, Turn):-
    length(Edge_Points, Num_Pts), Num_Pts >= 10,
    % if points are enough for fitting an ellipse
    fit_elps_2d(Edge_Points, Center, Parameters),
    reorder_points_clockwise(Center, Edge_Points, Edge_Points_1),
    eval_object_elps(Img, Model, Edge_Points_1,
                     [Center, Parameters], Obj, Turn), !.

eval_object_elps(Img, Model, Edge_Points, [Center, Parameters], Obj, Turn):-
    Edge_Points = [First | Tail],
    append(Tail, [First], EP2),
    %concurrent_maplist(sample_middle_point(Img, Model, Center), Edge_Points, EP2, Sampled),
    concurrent_maplist(sample_middle_point_on_elps(Img, Model, elps(Center, Parameters)), Edge_Points, EP2, Sampled),
    concurrent_maplist(dist_elps_point_2d([Center, Parameters]), Sampled, Distances),
    size_2d(Img, W, H),
    
    /*%% debug
    clone_img(Img, Img2),
    ellipse_points_2d(Center, Parameters, [W, H], ELPS),
    draw_points_2d(Img2, ELPS, red),
    draw_points_2d(Img2, Edge_Points, blue),
    draw_points_2d(Img2, Sampled, yellow),
    showimg_win(Img2, 'fit elps'),
    release_img(Img2),
    %% Debug end*/

    min_error_dist_ratio(Err_R), min_error_prob(Prob),
    Err is sqrt(W**2 + H**2)*Err_R,
    findall(D, (member(D, Distances), greater_than(Err, D)), Ds),
    length(Ds, Len), length(Distances, Tot),
    (Len/Tot >= Prob ->
         (Obj = elps(Center, Parameters), !);
     (last(Sampled, SP), append([SP], Sampled, Sampled1),
      last(Distances, DS), append([DS], Distances, Distances1),
      remove_failed_points(Edge_Points, Sampled1, Distances1, Err, Trusted),
      Turn1 is Turn + 1,
      abduce_object_elps(Img, Model, Trusted, Obj, Turn1))
    ), !.

% circle
abduce_object_circle(Img, Model, _, Obj, 5):-
    abduce_object_circle(Img, Model, [], Obj, 0), !.
abduce_object_circle(Img, Model, Edge_Points, Obj, Rep):-
    length(Edge_Points, Num_Pts), Num_Pts =< 1,
    size_2d(Img, W, H),
    rand_line_2d([W, H], Pt, Dir),
    sample_line_color_L_hist_2d(Img, Pt, Dir, Line_Pts, Line_Hist),
    predict_svm(Model, Line_Hist, Line_Label),
    R1 is Rep + 1,
    ((positive_segs(Line_Pts, Line_Label, PSegs),
      (random_select(PSegs, Seg))) -> % only use one seg
         (abduce_object_circle(Img, Model, Seg, Obj, R1), !);
     (abduce_object_circle(Img, Model, [], Obj, R1), !)).

abduce_object_circle(Img, Model, Edge_Points, Obj, Rep):-
    length(Edge_Points, Num_Pts),
    Num_Pts < 3, Num_Pts > 1, randset(2, Num_Pts, Indices),
    index_select1(Indices, Edge_Points, [P1, P2]),
    % randomly choose clockwise or inverse clockwise
    mid_point(P1, P2, P0),
    R1 is Rep + 1,
    ((sample_middle_point_between(Img, Model, P0, P1, P2, P), P \= []) ->
         (abduce_object_circle(Img, Model, [P | Edge_Points], Obj, R1),
          !);
     (list_delete(Edge_Points, [P1, P2], Edge_Points_1),
      abduce_object_circle(Img, Model, Edge_Points_1, Obj, R1), !)
    ).

abduce_object_circle(Img, Model, Edge_Points, Obj, Rep):-
    length(Edge_Points, Num_Pts), Num_Pts >= 3,
    % if points are enough for fitting an ellipse
    fit_circle_2d(Edge_Points, Center, Radius),
    reorder_points_clockwise(Center, Edge_Points, Edge_Points_1),
    eval_object_circle(Img, Model, Edge_Points_1, [Center, Radius], Obj, Rep),
    !.

eval_object_circle(Img, Model, Edge_Points, [Center, Radius], Obj, Rep):-
    % Edge_Points = [_ | Tail],
    /*
    Edge_Points = [_ | Tail], append(Head, [_], Edge_Points), !,
    concurrent_maplist(eval_middle_point(Img, Model, circle(Center, Radius)),
            Head, Tail, Sampled),
    */
    rec_mid_point(Img, Model, circle(Center, Radius),
                  Edge_Points, Sampled, 2),
    eval_fit(Img, circle(Center, Radius), Sampled, Rate),
    min_error_prob_circle(Prob),
    %(Rate =:= 0 ->
    %     abduce_object_circle(Img, Model, [], Obj)), !,
    (Rate >= Prob ->
         (Obj_ = circle(Center, Radius),
          Center = [X, Y],
          size_2d(Img, W, H),
          ((X < W, X >= 0, Y < H, Y >= 0,
            % add the learned predicate reasonable_fit/3
            reasonable_circle_fit(Img, Model, Obj_)) ->
               (Obj = Obj_, !);
           (abduce_object_circle(Img, Model, [], Obj, 0), !)
          ), !);
     (append(Sampled, Sampled, SS),
      SS = [A, B| Rest],
      %writeln(longest_curve(Rest, [B, A], 361, [], Longest)),
      longest_curve(Rest, [B, A], 361, [], Longest),

      /*%% debug
      clone_img(Img, Img2),
      draw_points_2d(Img2, Longest, red),
      showimg_win(Img2, 'fit elps'),
      release_img(Img2),
      %% Debug end*/
      R1 is Rep + 1,
      abduce_object_circle(Img, Model, Longest, Obj, R1), !)
    ), !.
     /*
     (append(Edge_Points1, [_], Tail), !,
      remove_failed_points(Edge_Points1, Sampled, Distances, Err, Trusted0),
      Sampled = [S0 | _], append([S0], Trusted0, Trusted1),
      list_to_set(Trusted1, Trusted),
      abduce_object_circle(Img, Model, Trusted, Obj))
    ), !.
     */

eval_fit(Img, circle(Center, _), _, 0.0):-
    size_2d(Img, W, H),
    Center = [X, Y],
    (X < 0; Y < 0; X >= W; Y >= H), !.
eval_fit(Img, circle(Center, Radius), Sampled, Rate):-
    concurrent_maplist(dist_circle_point_2d([Center, Radius]), Sampled, Distances),
    Distances \= [],
    /*%% debug
    size_2d(Img, W, H),
    clone_img(Img, Img2),
    circle_points_2d(Center, Radius, [W, H], CIRCLE),
    draw_points_2d(Img2, CIRCLE, red),
    % draw_points_2d(Img2, Edge_Points, blue),
    draw_points_2d(Img2, Sampled, yellow),
    showimg_win(Img2, 'fit elps'),
    release_img(Img2),
    %% Debug end*/

    size_2d(Img, W, H),
    min_error_dist_ratio(Err_R),
    Err is sqrt(W**2 + H**2)*Err_R,
    findall(D, (member(D, Distances), greater_than(Err, D)), Ds),
    length(Ds, Len), length(Distances, Tot),
    divide0(Len, Tot, Rate), !.
eval_fit(_, _, _, 0):-
    !.

% recursive sample middlepoint
rec_mid_point(_, _, _, Sampled, Sampled, 0):-
    !.
rec_mid_point(Img, Model, circle(Center, Radius), Edge_Points, Sampled, N):-
    Edge_Points = [_ | Tail], append(Head, [_], Edge_Points), !,
    maplist(eval_middle_point(Img, Model, circle(Center, Radius)),
            Head, Tail, Sampled1_),
    append(Sampled1_, Sampled1),
    N1 is N - 1,
    list_to_set(Sampled1, Edge_Points1_),
    list_delete(Edge_Points1_, [[]], Edge_Points1),
    rec_mid_point(Img, Model, circle(Center, Radius),
                  Edge_Points1, Sampled, N1).

% longest curve
longest_curve([], _, _, Re1, Re):-
    reverse(Re1, Re), !.
% longest_curve([_], _, _, Re1, Re):-
%     !.
% longest_curve([_, _], _, _, Re, Re):-
%     !.
% longest_curve([_, _, _], _, _, Re, Re):-
%     !.
longest_curve([C | Ps], Current, 361, Temp, Re):-
    Current = [B, A | _],
    vec_diff(A, B, V1), vec_diff(B, C, V2),
    vec_rotate_angle(V1, V2, Ang),
    abs(Ang) < 30,
    append([C], Current, Current1),
    longest_curve(Ps, Current1, Ang, Temp, Re), !.
longest_curve([C | Ps], Current, Last_Ang, Temp, Re):-
    Current = [B, A | _],
    vec_diff(A, B, V1), vec_diff(B, C, V2),
    vec_rotate_angle(V1, V2, Ang),
    sign(Ang) =:= sign(Last_Ang), % same direction
    abs(Ang) > 2, abs(Ang) < 30, % no big turning
    append([C], Current, Current1),
    longest_curve(Ps, Current1, Ang, Temp, Re), !.
longest_curve([C | Ps], Current, _, Temp, Re):-
    Current = [B | _],
    length(Current, LC), length(Temp, LT),
    (LC >= LT ->
         longest_curve(Ps, [C, B], 361, Current, Re);
     longest_curve(Ps, [C, B], 361, Temp, Re)
    ), !.
    

% remove the points failing the evaluation
remove_failed_points([], [_], [_], _, []):-
    !.
remove_failed_points([EP | Edge_Points],
                     [_, S2 | Sampled], [D1, D2 | Distances], Err,
                     [T | Trusted]):-
    D1 =< Err, D2 =< Err, T = EP,
    remove_failed_points(Edge_Points,
                         [S2 | Sampled], [D2 | Distances], Err, Trusted), !.
remove_failed_points([_ | Edge_Points],
                     [_, S2 | Sampled], [_, D2 | Distances], Err,
                     [T | Trusted]):-
    S2 \= [], T = S2,
    remove_failed_points(Edge_Points,
                         [S2 | Sampled], [D2 | Distances], Err, Trusted), !.
remove_failed_points([_ | Edge_Points],
                     [_, S2 | Sampled], [_, D2 | Distances], Err,
                     Trusted):-
    S2 = [],
    remove_failed_points(Edge_Points,
                         [S2 | Sampled], [D2 | Distances], Err, Trusted), !.

% may sample clockwise or inverse clockwise
sample_middle_point(Img, Model, Center, P1_, P2_, Edge_Point):-
    (maybe(0.5) -> (P1 = P2_, P2 = P1_); (P1 = P1_, P2 = P2_)), !,
    vec_diff(P1, Center, D1),
    vec_diff(P2, Center, D2),
    vec_rotate_angle_clockwise(D1, D2, Ang1), Ang is Ang1/2,
    turn_degree_2d(D1, Ang, Dir),
    %sample_ray_color_L_hist_2d(Img, Center, Dir, Ray_Pts, Ray_Hist),
    ((sample_ray_color_L_hist_2d(Img, Center, Dir, Ray_Pts, Ray_Hist),
      predict_svm(Model, Ray_Hist, Ray_Label),
      Ray_Label = [1, 1, 1 | _], % center bust be foreground
      positive_segs(Ray_Pts, Ray_Label, Segs),
      crossed_segs_2d(Center, Segs, [[Center, EP] | _])) ->
         Edge_Point = EP;
     Edge_Point = []), !.

sample_middle_point_on_elps(Img, Model, elps(Center, Para),
                            P1_, P2_, Edge_Point):-
    (maybe(0.5) -> (P1 = P2_, P2 = P1_); (P1 = P1_, P2 = P2_)), !,
    (eval_point_between_clockwise(Img, Model, elps(Center, Para),
                                  P1, P2, 0.5, 10, EP) ->
         Edge_Point = EP;
     Edge_Point = []), !.

sample_middle_point_between(Img, Model, Center, P1_, P2_, Edge_Point):-
    (maybe(0.5) -> (P1 = P2_, P2 = P1_); (P1 = P1_, P2 = P2_)), !,
    vec_diff(P1, Center, D1),
    vec_diff(P2, Center, D2),
    vec_rotate_angle(D1, D2, Ang1), Ang is Ang1/2,
    turn_degree_2d(D1, Ang, Dir),
    %sample_ray_color_L_hist_2d(Img, Center, Dir, Ray_Pts, Ray_Hist),
    ((sample_ray_color_L_hist_2d(Img, Center, Dir, Ray_Pts, Ray_Hist),
      predict_svm(Model, Ray_Hist, Ray_Label),
      %Ray_Label = [1, 1, 1 | _], % occlusion happens, center doesn't need to be foreground
      positive_segs(Ray_Pts, Ray_Label, Segs),
      Segs = [[_, EP] | _]) ->
         Edge_Point = EP;
     Edge_Point = []), !.

eval_middle_point(Img, Model, circle(Cen, Rad), P1, P2, [P1, Edge_Point, P2]):-
    %(maybe(0.001) -> (P1 = P2_, P2 = P1_); (P1 = P1_, P2 = P2_)), !,
    random(0.4, 0.6, Ratio),
    eval_point_between(Img, Model, circle(Cen, Rad), P1, P2,
                       Ratio, 10, Edge_Point).
    %vec_diff(P1, Cen, D1),
    %vec_diff(P2, Cen, D2),
    %vec_rotate_angle(D1, D2, Ang1), Ang is Ang1/2,
    %turn_degree_2d(D1, Ang, Dir),
    %vec_sum(Cen, Dir, Sum),
    %%sample_ray_color_L_hist_2d(Img, Cen, Dir, Ray_Pts, Ray_Hist),
    %((sample_ray_color_L_hist_2d(Img, Cen, Dir, Ray_Pts, Ray_Hist),
    %  predict_svm(Model, Ray_Hist, Ray_Label),
    %  positive_segs(Ray_Pts, Ray_Label, Segs),
    %  dist_point_circle_2d(Sum, [Cen, Rad], _, EP),
    %  column(2, Segs, Ends),
    %  closest_point_in_list(EP, Ends, P)) ->
    %     Edge_Point = P;
    % Edge_Point = []), !.

/*
eval_middle_point(Img, Model, elps(Cen, Param), P1_, P2_, Edge_Point):-
    (maybe(0.5) -> (P1 = P2_, P2 = P1_); (P1 = P1_, P2 = P2_)), !,
    vec_diff(P1, Center, D1),
    vec_diff(P2, Center, D2),
    vec_rotate_angle(D1, D2, Ang1), Ang is Ang1/2,
    turn_degree_2d(D1, Ang, Dir),
    % TODO::: to be optimized, x/y = sin/cos(theta)*k, solve equation
    size_2d(Img, W, H),
    sample_ray_color_L_hist_2d(Img, Center, Dir, Ray_Pts, Ray_Hist),
    ellipse_points_2d(Cen, Para, [W, H], Elps_Pts),
    ((predict_svm(Model, Hist, Ray_Label),
      intersection(Ray_Pts, Elps_Pts, [EP | _]),
      pts_color_hists_2d(Img, [EP], Hist)) ->
         Edge_Point = EP;
     Edge_Point = []), !.
*/

% reorder the points (Pts_) w.r.t. a origin P0 clockwise
reorder_points_clockwise(P0, Pts_, Re):-
    list_to_set(Pts_, Pts),
    concurrent_maplist(vec_neg_diff(P0), Pts, Dirs),
    concurrent_maplist(vec_rotate_angle_clockwise([0, -1]), Dirs, Angs),
    pairs_keys_values(Pairs, Angs, Pts),
    keysort(Pairs, Sorted),
    pairs_keys_values(Sorted, _, Re).

crossed_segs_2d(_, [], []):-
    !.
crossed_segs_2d([X, Y], [S | Ss], [S | CSs]):-
    S = [[X1, Y1], [X2, Y2]],
    ((between(X1, X2, X); between(X2, X1, X)),
     (between(Y1, Y2, Y); between(Y2, Y1, Y))), !,
    crossed_segs_2d([X, Y], Ss, CSs).
crossed_segs_2d(P, [_ | Ss], CSs):-
    crossed_segs_2d(P, Ss, CSs).

%=======================================================================
% given an item sequence and a label (0-negative, 1-positive) sequence,
% get the subsequences that are + (1s)
%=======================================================================
positive_segs(Pts, Labels, Return):-
    positive_segs(Pts, Labels, out, nil, Return).
% positive_seg(Pts, Labels, In/Out, Tmp_Start, Return)
positive_segs([], [], _, _, []):-
    !.
positive_segs([_, _  | Pts], [1, 0 | Labels], out, nil, Re):-
    positive_segs(Pts, Labels, out, nil, Re), !.
positive_segs([_, _, _ | Pts], [1, 1, 0 | Labels], out, nil, Re):-
    positive_segs(Pts, Labels, out, nil, Re), !.
positive_segs([_, _, _, _ | Pts], [1, 1, 1, 0 | Labels], out, nil, Re):-
    positive_segs(Pts, Labels, out, nil, Re), !.
positive_segs([P | Pts], [1 | Labels], out, nil, Re):-
    positive_segs(Pts, Labels, in, P, Re), !.
positive_segs([_ | Pts], [0 | Labels], out, nil, Re):-
    positive_segs(Pts, Labels, out, nil, Re), !.
positive_segs([_, _ | Pts], [0, 1 | Labels], in, Tmp_Start, Re):-
    positive_segs(Pts, Labels, in, Tmp_Start, Re), !.
positive_segs([_, _, _ | Pts], [0, 0, 1 | Labels], in, Tmp_Start, Re):-
    positive_segs(Pts, Labels, in, Tmp_Start, Re), !.
positive_segs([_, _, _, _ | Pts], [0, 0, 0, 1 | Labels], in, Tmp_Start, Re):-
    positive_segs(Pts, Labels, in, Tmp_Start, Re), !.
positive_segs([P | Pts], [0 | Labels], in, Tmp_Start, [R | Re]):-
    R = [Tmp_Start, P],
    positive_segs(Pts, Labels, out, nil, Re).
positive_segs([_ | Pts], [1 | Labels], in, Tmp_Start, Re):-
    positive_segs(Pts, Labels, in, Tmp_Start, Re), !.

%======================================
% train statistical model
%======================================
train_stat_model_protist(Model):-
    time(gen_pts_train_data('../../data/Protist1.jpg',
                            '../../data/Protist1_fg.bmp',
                            100, Data_Label_1)),
    time(gen_pts_train_data('../../data/Protist2.jpg',
                            '../../data/Protist2_fg.bmp',
                            100, Data_Label_2)),
    time(gen_pts_train_data('../../data/Protist3.jpg',
                            '../../data/Protist3_fg.bmp',
                            100, Data_Label_3)),
    time(gen_pts_train_data('../../data/Protist4.jpg',
                            '../../data/Protist4_fg.bmp',
                            100, Data_Label_4)),
    time(gen_pts_train_data('../../data/Protist5.jpg',
                            '../../data/Protist5_fg.bmp',
                            100, Data_Label_5)),
    %print_list_ln(Data_Label),
    append([Data_Label_1, Data_Label_2, Data_Label_3, Data_Label_4, Data_Label_5], Data_Labels),
    subsample(1.0, 0.3, Data_Labels, Data_Label),
    write("Training SVM: "),
    time(train_svm(Data_Label,
                   '-g 0.0039 -c 100000 -h 0',
                   Model)),
    writeln("Training SVM complete!"),
    save_model_svm(Model, '../../tmp/SVM_Protist.model').
train_stat_model_moon(Model):-
    time(gen_pts_train_data('../../data/Moon0.jpg',
                            '../../data/Moon0_fg.bmp',
                            100, Data_Label_1)),
    time(gen_pts_train_data('../../data/Moon1.jpg',
                            '../../data/Moon1_fg.bmp',
                            100, Data_Label_2)),
    time(gen_pts_train_data('../../data/Moon2.jpg',
                            '../../data/Moon2_fg.bmp',
                            100, Data_Label_3)),
    time(gen_pts_train_data('../../data/Moon3.jpg',
                            '../../data/Moon3_fg.bmp',
                            100, Data_Label_4)),
    time(gen_pts_train_data('../../data/Moon4.jpg',
                            '../../data/Moon4_fg.bmp',
                            100, Data_Label_5)),
    time(gen_pts_train_data('../../data/Moon5.jpg',
                            '../../data/Moon5_fg.bmp',
                            100, Data_Label_6)),
    time(gen_pts_train_data('../../data/Moon6.jpg',
                            '../../data/Moon6_fg.bmp',
                            200, Data_Label_7)),
    %print_list_ln(Data_Label),
    append([Data_Label_1, Data_Label_2, Data_Label_3, Data_Label_4, Data_Label_5, Data_Label_6, Data_Label_7], Data_Labels),
    subsample(1.0, 0.7, Data_Labels, Data_Label),
    write("Training SVM: "),
    time(train_svm(Data_Label, '-g 0.0039 -c 100000 -h 0', Model)),
    writeln("Training SVM complete!"),
    save_model_svm(Model, '../../tmp/SVM_Moon.model').

train_stat_model_crater(Model):-
    time(gen_pts_train_data('../../data/crater.png',
                            '../../data/crater_fg.bmp',
                            500, Data_Labels)),
    subsample(1.0, 0.4, Data_Labels, Data_Label),
    write("Training SVM: "),
    time(train_svm(Data_Label, '-g 0.0039 -c 100000 -h 0', Model)),
    writeln("Training SVM complete!"),
    save_model_svm(Model, '../../tmp/SVM_crater.model').


%=========================
% cut object into 2 halves
%=========================
% get the splited points
% clock angle \in [0, 11]
split_ellipse(Img, elps(Cen, Param), Clock_Ang, Front, Rear):-
    size_2d(Img, W, H),
    get_points_in_ellipse_2d([Cen, Param], [W, H], Points),
    Theta is (Clock_Ang mod 12)*30*pi/180,
    DX is cos(Theta), DY is sin(Theta),
    findall(P,
            (member(P, Points),
             vec_diff(P, Cen, D),
             cross([DX, DY], D, C),
             C < 0),
            Front
           ),
    findall(P,
            (member(P, Points),
             vec_diff(P, Cen, D),
             cross([DX, DY], D, C),
             C > 0),
            Rear
           ).

split_circle(Img, circle(Cen, Rad), Clock_Ang, Front, Rear):-
    size_2d(Img, W, H),
    get_points_in_circle_2d([Cen, Rad], [W, H], Points),
    Theta is (Clock_Ang mod 12)*30*pi/180,
    DX is cos(Theta), DY is sin(Theta),
    findall(P,
            (member(P, Points),
             in_canvas(P, Img),
             vec_diff(P, Cen, D),
             cross([DX, DY], D, C),
             C < 0),
            Front
           ),
    findall(P,
            (member(P, Points),
             in_canvas(P, Img),
             vec_diff(P, Cen, D),
             cross([DX, DY], D, C),
             C > 0),
            Rear
           ).

%===================================
% find largest contrast direction
%===================================
get_largest_contrast_angle(Img, Obj, Re):-
    get_largest_contrast_angle(Img, Obj, 12, -1, -1000, Re).
get_largest_contrast_angle(_, _, 0, Re, _, Re):-
    !.
get_largest_contrast_angle(Img, elps(C, P), Ang, Tmp_Ang, Tmp, Re):-
    Ang1 is Ang - 3, % Four directions
    split_ellipse(Img, elps(C, P), Ang, Front, Rear),
    pts_color_L_avg_2d(Img, Front, Bright_F),
    pts_color_L_avg_2d(Img, Rear, Bright_R),
    Diff is Bright_F - Bright_R,
    (Diff > Tmp ->
         (get_largest_contrast_angle(Img, elps(C, P), Ang1, Ang, Diff, Re), !);
     (get_largest_contrast_angle(Img, elps(C, P), Ang1, Tmp_Ang, Tmp, Re), !)
    ).
get_largest_contrast_angle(Img, circle(C, R), Ang, Tmp_Ang, Tmp, Re):-
    Ang1 is Ang - 3, % Four directions
    split_circle(Img, circle(C, R), Ang, Front, Rear),
    pts_color_L_avg_2d(Img, Front, Bright_F),
    pts_color_L_avg_2d(Img, Rear, Bright_R),
    Diff is Bright_F - Bright_R,
    (Diff > Tmp ->
         (get_largest_contrast_angle(Img, circle(C, R), Ang1, Ang, Diff, Re), !);
     (get_largest_contrast_angle(Img, circle(C, R), Ang1, Tmp_Ang, Tmp, Re), !)
    ).

%=============================================
% mid point evaluation for elps and circle
%=============================================
eval_point_between(Img, Model, Obj, P1, P2, Ratio, Range, Edge_Point):-
    arc_point_between(Obj, P1, P2, Ratio, Sampled),
    (Obj = elps(Center, _); Obj = circle(Center, _)), !,
    edge_point_in_dir_range(Img, Model, Center, Sampled, Range, Edge_Point).

eval_point_between_clockwise(Img, Model, Obj, P1, P2,
                             Ratio, Range, Edge_Point):-
    arc_point_between_clockwise(Obj, P1, P2, Ratio, Sampled),
    (Obj = elps(Center, _); Obj = circle(Center, _)), !,
    edge_point_in_dir_range(Img, Model, Center, Sampled, Range, Edge_Point).

edge_point_in_dir_range(Img, Model, Center, Point, Range, Edge_Point):-
    Point = [SX, SY],
    (not(in_canvas(Point, Img)) ->
         (Edge_Point = [], !);
     (vec_diff(Point, Center, [DSX_, DSY_]),
      M is DSX_**2 + DSY_**2,
      (M =:= 0 ->
           (Edge_Point = [], !);
       (DSX is DSX_/sqrt(M),
        DSY is DSY_/sqrt(M),
      
        S1X is round(SX + Range*DSX),
        S2X is round(SX - Range*DSX),
        S1Y is round(SY + Range*DSY),
        S2Y is round(SY - Range*DSY),
        
        size_2d(Img, W, H),
        line_seg_points_2d([S2X, S2Y], Point, [W, H], Seg1),
        line_seg_points_2d(Point, [S1X, S1Y], [W, H], [_ |Seg2]),
        append(Seg1, Seg2, Seg),
        
        pts_color_L_hists_2d(Img, Seg, Hists),
        predict_svm(Model, Hists, Labels),
        has_edge_point_outward(Labels, Seg, Edge_Point)), !)
      )
    ), !.

% has edge point
has_edge_point_outward([_], _, []):-
    !.
has_edge_point_outward([1, 0 | _], [Re, _ | _], Re):-
    !.
has_edge_point_outward([_, X | Xs], [_, P | Ps], Re):-
    has_edge_point_outward([X | Xs], [P | Ps], Re), !.

has_edge_point_inward([_], _, []):-
    !.
has_edge_point_inward([0, 1 | _], [_, Re | _], Re):-
    !.
has_edge_point_inward([_, X | Xs], [_, P | Ps], Re):-
    has_edge_point_inward([X | Xs], [P | Ps], Re), !.

has_edge_point(Xs, Ps, Re):-
    (has_edge_point_inward(Xs, Ps, Re);
     has_edge_point_outward(Xs, Ps, Re)), !.

%=======================================
% reasonable circle fit, learned by MIL
%=======================================
reasonable_circle_fit(Img, Model, circle(Cent, Rad)):-
    edge_point_proportion(Img, Model, circle(Cent, Rad), Prop),
    %writeln(Prop),
    Prop >= 0.4.
edge_point_proportion(Img, Model, circle(Cent, Rad), Prop):-
    eval_edge_points(Img, Model, circle(Cent, Rad), 36, EPs),
    length(EPs, Len),
    Prop is Len/36.

eval_edge_points(_, _, circle(_, _), 0, []):-
    !.
eval_edge_points(Img, Model, circle(Cent, Rad), Num, [EP | EPs]):-
    V0 = [100, 0], 
    Sep is 10.0, Ang is Num*Sep,
    vec_rotate_angle_clockwise(V0, V1, Ang),
    vec_sum(V0, Cent, P0), vec_sum(V1, Cent, P1),
    arc_point_between(circle(Cent, Rad), P0, P1, 1.0, P),
    % has edge point on fitted edge
    in_canvas(P, Img),
    edge_point_in_dir_range(Img, Model, Cent, P, 5, EP),
    EP \= [],
    % continue
    Num1 is Num - 1,
    eval_edge_points(Img, Model, circle(Cent, Rad), Num1, EPs), !.
eval_edge_points(Img, Model, circle(Cent, Rad), Num, EPs):-
    Num1 is Num - 1,
    eval_edge_points(Img, Model, circle(Cent, Rad), Num1, EPs), !.

in_canvas(P, Img):-
    size_2d(Img, W_, H_),
    W is W_ - 1, H is H_ - 1,
    in_box([[0, 0], [W, H]], P).

