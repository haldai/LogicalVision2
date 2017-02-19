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
along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
************************************************************************/
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
