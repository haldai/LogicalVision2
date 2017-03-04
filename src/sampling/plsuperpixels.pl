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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prolog module for super pixels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-ensure_loaded(['../utils/utils.pl',
                 '../utils/geometry.pl']).

%=========================
% SuperPixel bounding box
%=========================
sp_box(SP, ID, Box):-
    get_sp_pixels(SP, ID, PTS),
    column(1, PTS, X), column(2, PTS, Y),
    max_list(X, R), min_list(X, L),
    max_list(Y, B), min_list(Y, T),
    Box = [[L, T], [R, B]].

% bounding box of multiple superpixels
sps_box(SP, IDs, Box):-
    get_sps_pixels(SP, IDs, PTS),
    column(1, PTS, X), column(2, PTS, Y),
    max_list(X, R), min_list(X, L),
    max_list(Y, B), min_list(Y, T),
    Box = [[L, T], [R, B]].

%=====================================
% line sampling inside of superpixel
%=====================================
% +Img: image; +SP: superpixels; +[IDs]: list of superpixel IDs;
% +Pt: Point-of-Line; +Dir: direction of line;
% -SegsPts: [[Seg1],[Seg2],...] Line points of segments in superpixels
sp_sample_line(SP, ID, Pt, Dir, SegsPts):-
    sps_sample_line(SP, [ID], Pt, Dir, SegsPts).
sps_sample_line(SP, IDs, Pt, Dir, SegsPts):-
    get_sps_pixels(SP, IDs, SPts),
    member(Pt, SPts), !, % point is in superpixel
    sps_box(SP, IDs, [TL, BR]), % bounding box of superpixel
    line_points_2d(Pt, Dir, TL, BR, LinePts1),
    % filter out points that not in superpixels
    intersection(LinePts1, SPts, Intsct),
    connected_components(Intsct, Comps),
    % filter out small segs that less than 5 pixels
    remove_small_lists(Comps, 5, SegsPts).

sp_sample_rand_line(SP, ID, SegsPts):-
    sps_sample_rand_line(SP, [ID], SegsPts).
sps_sample_rand_line(SP, IDs, SegsPts):-
    get_sps_pixels(SP, IDs, SPts),
    random_select(Pt, SPts, _), rand_2d_angle_vec(Dir),
    sps_sample_line(SP, IDs, Pt, Dir, SegsPts).

remove_small_lists([], _, []):-
    !.
remove_small_lists([S | Segs], Thresh, [S | Re]):-
    length(S, L), L > Thresh,
    remove_small_lists(Segs, Thresh, Re), !.
remove_small_lists([_ | Segs], Thresh, Re):-
    remove_small_lists(Segs, Thresh, Re), !.

%==================================================
% color transitions on random lines in superpixel
%==================================================
sp_rand_line_color_trans(Img, SP, ID, Colors):-
    sps_rand_line_color_trans(Img, SP, [ID], Colors).
sps_rand_line_color_trans(Img, SP, IDs, Colors):-
    sps_sample_rand_line(SP, IDs, SegsPts),
    ball_segs_transition(Img, SegsPts, Colors).

% multiple times
sp_rand_lines_color_trans(Img, SP, ID, N, Colors):-
    sps_rand_lines_color_trans(Img, SP, [ID], N, Colors).
sps_rand_lines_color_trans(_, _, _, 0, []):-
    !.
sps_rand_lines_color_trans(Img, SP, IDs, N, Colors):-
    sps_rand_line_color_trans(Img, SP, IDs, C1),
    N1 is N - 1,
    sps_rand_lines_color_trans(Img, SP, IDs, N1, C2),
    append(C1, C2, Colors), !.

%% for debug, outputs segment points either
sps_rand_line_color_trans(Img, SP, IDs, SegsPts, Colors):-
    sps_sample_rand_line(SP, IDs, SegsPts),
    ball_segs_transition(Img, SegsPts, Colors).

ball_segs_transition(Img, Segs, Re):-
    ball_segs_transition(Img, Segs, [], Re).
ball_segs_transition(_, [], Re, Re):-
    !.
ball_segs_transition(Img, [S | Segs], Tmp, Re):-
    ball_color_transition(Img, S, nothing, Tran),
    append(Tmp, [Tran], Tmp1),
    ball_segs_transition(Img, Segs, Tmp1, Re).

ball_color_transition(_, [], _, []):-
    !.
ball_color_transition(Img, [P | Pts], green, Re):-
    point_color_2d(Img, P, Lab),
    color(Lab, green), !,
    ball_color_transition(Img, Pts, green, Re), !.
ball_color_transition(Img, [P | Pts], Last_Color, [green | Re]):-
    point_color_2d(Img, P, Lab),
    color(Lab, green), !,
    Last_Color \= green,
    ball_color_transition(Img, Pts, green, Re), !.
ball_color_transition(Img, [P | Pts], Last_Color, Re):-
    point_color_2d(Img, P, Lab),
    color(Lab, Last_Color), !,
    ball_color_transition(Img, Pts, Last_Color, Re), !.
ball_color_transition(Img, [P | Pts], Last_Color, [Color | Re]):-
    point_color_2d(Img, P, Lab),
    color(Lab, Color), !, Color \= Last_Color,
    ball_color_transition(Img, Pts, Color, Re), !.
ball_color_transition(Img, [_ | Pts], Last_Color, Re):-
    ball_color_transition(Img, Pts, Last_Color, Re), !.
