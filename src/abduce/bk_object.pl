% background knowledge for abducing objects
%============================================================================

% circles, ellipses, polygons
% confidence level (mid-point evaluation)

min_error_dist(5).
min_error_prob(0.7).

:- ensure_loaded(['../utils/utils.pl']).

abduce_object_elps(Img, Model, Edge_Points, Obj):-
    length(Edge_Points, Num_Pts), Num_Pts =< 1,
    size_2d(Img, W, H),
    rand_line_2d([W, H], Pt, Dir),
    sample_line_color_L_hist_2d(Img, Pt, Dir, Line_Pts, Line_Hist),
    predict_svm(Model, Line_Hist, Line_Label),
    ((positive_segs(Line_Pts, Line_Label, PSegs),
      (random_select(PSegs, Seg))) -> % only use one seg
         (abduce_object_elps(Img, Model, Seg, Obj), !);
     (abduce_object_elps(Img, Model, [], Obj), !)).

abduce_object_elps(Img, Model, Edge_Points, Obj):-
    length(Edge_Points, Num_Pts),
    Num_Pts < 10, Num_Pts > 1, randset(2, Num_Pts, Indices),
    index_select1(Indices, Edge_Points, [P1, P2]),
    % randomly choose clockwise or inverse clockwise
    mid_point(P1, P2, P0),
    /*
    (maybe(0.2) -> (P1 = P2_, P2 = P1_); (P1 = P1_, P2 = P2_)), !,
    
    vec_diff(P1, P0, D1), vec_diff(P2, P0, D2),
    vec_rotate_angle_clockwise(D1, D2, Ang1), Ang is Ang1/2,
    turn_degree_2d(D1, Ang, Dir),
    sample_ray_color_L_hist_2d(Img, P0, Dir, Ray_Pts, Ray_Hist),
    ((predict_svm(Model, Ray_Hist, Ray_Label),
      Ray_Label = [1, 1, 1 | _],
      positive_segs(Ray_Pts, Ray_Label, Segs),
      crossed_segs_2d(P0, Segs, [[P0, P] | _])) ->
    */
    ((sample_middle_point(Img, Model, P0, P1, P2, P), P \= []) ->
               (abduce_object_elps(Img, Model, [P | Edge_Points], Obj), !);
     (list_delete(Edge_Points, [P1, P2], Edge_Points_1),
      abduce_object_elps(Img, Model, Edge_Points_1, Obj), !)
    ).

abduce_object_elps(Img, Model, Edge_Points, Obj):-
    length(Edge_Points, Num_Pts), Num_Pts >= 10,
    % if points are enough for fitting an ellipse
    fit_elps_2d(Edge_Points, Center, Parameters),
    reorder_points_clockwise(Center, Edge_Points, Edge_Points_1),
    eval_object_elps(Img, Model, Edge_Points_1, [Center, Parameters], Obj), !.

eval_object_elps(Img, Model, Edge_Points, [Center, Parameters], Obj):-
    Edge_Points = [First | Tail],
    append(Tail, [First], EP2),
    maplist(sample_middle_point(Img, Model, Center), Edge_Points, EP2, Sampled),
    maplist(dist_elps_point_2d([Center, Parameters]), Sampled, Distances),

    %% debug
    clone_img(Img, Img2),
    size_2d(Img, W, H),
    ellipse_points_2d(Center, Parameters, [W, H], ELPS),
    draw_points_2d(Img2, ELPS, red),
    draw_points_2d(Img2, Edge_Points, blue),
    draw_points_2d(Img2, Sampled, yellow),
    showimg_win(Img2, 'fit elps'),
    release_img(Img2),
    %% Debug end

    min_error_dist(Err), min_error_prob(Prob),
    findall(D, (member(D, Distances), greater_than(Err, D)), Ds),
    length(Ds, Len), length(Distances, Tot),
    (Len/Tot >= Prob ->
         (Obj = elps(Center, Parameters), !);
     (last(Sampled, SP), append([SP], Sampled, Sampled1),
      last(Distances, DS), append([DS], Distances, Distances1),
      remove_failed_points(Edge_Points, Sampled1, Distances1, Trusted),
      abduce_object_elps(Img, Model, Trusted, Obj))
    ), !.

remove_failed_points([], [_], [_], []):-
    !.
remove_failed_points([EP | Edge_Points],
                     [_, S2 | Sampled], [D1, D2 | Distances], [T | Trusted]):-
    
    min_error_dist(Err),
    D1 =< Err, D2 =< Err, T = EP,
    remove_failed_points(Edge_Points,
                         [S2 | Sampled], [D2 | Distances], Trusted), !.
remove_failed_points([_ | Edge_Points],
                     [_, S2 | Sampled], [_, D2 | Distances], [T | Trusted]):-
    
    S2 \= [], T = S2,
    remove_failed_points(Edge_Points,
                         [S2 | Sampled], [D2 | Distances], Trusted), !.
remove_failed_points([_ | Edge_Points],
                     [_, S2 | Sampled], [_, D2 | Distances], Trusted):-
    S2 = [],
    remove_failed_points(Edge_Points,
                         [S2 | Sampled], [D2 | Distances], Trusted), !.

sample_middle_point(Img, Model, Center, P1_, P2_, Edge_Point):-
    (maybe(0.2) -> (P1 = P2_, P2 = P1_); (P1 = P1_, P2 = P2_)), !,
    vec_diff(P1, Center, D1),
    vec_diff(P2, Center, D2),
    vec_rotate_angle_clockwise(D1, D2, Ang1), Ang is Ang1/2,
    turn_degree_2d(D1, Ang, Dir),
    sample_ray_color_L_hist_2d(Img, Center, Dir, Ray_Pts, Ray_Hist),
    ((predict_svm(Model, Ray_Hist, Ray_Label),
      Ray_Label = [1, 1, 1 | _],
      positive_segs(Ray_Pts, Ray_Label, Segs),
      crossed_segs_2d(Center, Segs, [[Center, EP] | _])) ->
         Edge_Point = EP;
     Edge_Point = []), !.
    
% reorder the points (Pts_) w.r.t. a origin P0 clockwise
reorder_points_clockwise(P0, Pts_, Re):-
    list_to_set(Pts_, Pts),
    maplist(vec_neg_diff(P0), Pts, Dirs),
    maplist(vec_rotate_angle_clockwise([0, -1]), Dirs, Angs),
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
% train statistical model for protist
%======================================
train_stat_model(Model):-
    time(gen_pts_train_data('../../data/Protist0.png',
                            '../../data/Protist0_fg.bmp',
                            100, Data_Label_1)),
    %print_list_ln(Data_Label),
    subsample(1.0, 0.3, Data_Label_1, Data_Label),
    train_svm(Data_Label, Model).

%======================================================
% cut object into 2 halves and calculate the contrast
%======================================================
