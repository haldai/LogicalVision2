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
:- ensure_loaded(['../sampling/plsuperpixels.pl']).

%===========================================================
% [football] color transitions on random lines in superpixel
%===========================================================
sp_ball_rand_line_color_trans(Img, SP, ID, Colors):-
    sps_ball_rand_line_color_trans(Img, SP, [ID], Colors).
sps_ball_rand_line_color_trans(Img, SP, IDs, Colors):-
    sps_sample_rand_line(SP, IDs, SegsPts),
    ball_segs_transition(Img, SegsPts, Colors).

% multiple times
sp_ball_rand_lines_color_trans(Img, SP, ID, N, Colors):-
    sps_ball_rand_lines_color_trans(Img, SP, [ID], N, Colors).
sps_ball_rand_lines_color_trans(_, _, _, 0, []):-
    !.
sps_ball_rand_lines_color_trans(Img, SP, IDs, N, Colors):-
    sps_ball_rand_line_color_trans(Img, SP, IDs, C1),
    N1 is N - 1,
    sps_ball_rand_lines_color_trans(Img, SP, IDs, N1, C2),
    append(C1, C2, Colors), !.

% vertical
sp_ball_vertical_line_color_trans(Img, SP, ID, Colors):-
    sps_ball_rand_line_color_trans(Img, SP, [ID], Colors).
sps_ball_vertical_line_color_trans(Img, SP, IDs, Colors):-
    sps_sample_vertical_line(SP, IDs, SegsPts),
    ball_segs_transition(Img, SegsPts, Colors).

% multiple times
sp_ball_vertical_lines_color_trans(Img, SP, ID, N, Colors):-
    sps_ball_vertical_lines_color_trans(Img, SP, [ID], N, Colors).
sps_ball_vertical_lines_color_trans(_, _, _, 0, []):-
    !.
sps_ball_vertical_lines_color_trans(Img, SP, IDs, N, Colors):-
    sps_ball_vertical_line_color_trans(Img, SP, IDs, C1),
    N1 is N - 1,
    sps_ball_vertical_lines_color_trans(Img, SP, IDs, N1, C2),
    append(C1, C2, Colors), !.

%% for debug, outputs segment points either
sps_ball_rand_line_color_trans(Img, SP, IDs, SegsPts, Colors):-
    sps_sample_rand_line(SP, IDs, SegsPts),
    ball_segs_transition(Img, SegsPts, Colors).
sps_ball_vertical_line_color_trans(Img, SP, IDs, SegsPts, Colors):-
    sps_sample_vertical_line(SP, IDs, SegsPts),
    ball_segs_transition(Img, SegsPts, Colors).

% sampling superpixels
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

