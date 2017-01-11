:- ensure_loaded(['../sampling/plsampling.pl']).

%%%%%%%%%%%%%%%% training data %%%%%%%%%%%%%%%%%%%
% return Data + Label
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% generate data for training point classifier
gen_pts_train_data(Img_Path, Ground_Truth_Path, N, Data_Label):-
    load_img(Img_Path, Img), load_img(Ground_Truth_Path, Img_Truth),
    lines_pts_stat_data(Img, Img_Truth, N, Data_Label).
lines_pts_stat_data(_, _, 0, []):-
    !.
lines_pts_stat_data(Img, Img_Truth, N, Data_Label):-
    size_2d(Img, W, H),
    rand_line_2d([W, H], Pt, Dir),
    line_pts_train_data(Img, Img_Truth, Pt, Dir, Data_Label_1),
    N1 is N - 1,
    lines_pts_stat_data(Img, Img_Truth, N1, Data_Label_2),
    append(Data_Label_1, Data_Label_2, Data_Label).

line_pts_train_data(Img, Img_FG, Pt, Dir, Data_Label):-
    sample_line_color_L_2d(Img_FG, Pt, Dir, Line_pts, Labels_1),
    pts_color_L_hists_2d(Img, Line_pts, Hists),
    %pts_color_hists_2d(Img, Line_pts, Hists),
    maplist(greater_than_zero, Labels_1, Labels),
    pairs_keys_values(Data_Label, Hists, Labels).
