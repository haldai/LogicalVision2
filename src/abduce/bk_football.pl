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

%============
% fit circle
%============
find_ball(_, _, 0):-
    writeln("***no fit***"), !.
find_ball(ImgID, SP_IDs, N):-
    N > 0,
    fit_football(ImgID, SP_IDs, circle(Cen, _)),
    Dir = '../../data/MobileRobotAndBall1/raw_images',
    atomic_list_concat([Dir, ImgID], '/', ImgName),
    atomic_concat(ImgName, '.jpg', ImgPath),
    load_img(ImgPath, Img),
    size_2d(Img, W, H),
    (not(ground(Cen)) ->
         (release_img(Img), fail, !);
     true), !,
    Cen = [X, Y],
    (((X < 0; X >= W; Y < 0; Y >= H);
      (point_color_2d(Img, Cen, LAB),
       color(LAB, not(green)))) ->
         (writeln("***found***"),
          release_img(Img), !);
     (release_img(Img), fail, !)).
find_ball(ImgID, SP_IDs, N):-
    N1 is N - 1,
    find_ball(ImgID, SP_IDs, N1), !.

fit_football(ImgID, SP_IDs, circle(Cen, Rad)):-
    Dir = '../../data/MobileRobotAndBall1/raw_images',
    ODir = '../../out/SP',
    atomic_list_concat([Dir, ImgID], '/', ImgName),
    atomic_concat(ImgName, '.jpg', ImgPath),
    atomic_list_concat([ODir, 'Statistical', ImgID], '/', CSVName),
    atomic_concat(CSVName, '.csv', CSVPath),
    load_img(ImgPath, Img),
    time(load_superpixels(CSVPath, SP)),
    %% get "green -> non-green" and "non-green -> green" points
    findall(EP_List,
            (between(1, 20, _),
             sps_box_gng_pts(Img, SP, SP_IDs,EP_List)),
            EP_Lists),
    append(EP_Lists, Pts_),
    list_to_set(Pts_, Pts),
    (fit_circle_2d(Pts, Cen, Rad) ->
         (
             %% debug
             clone_img(Img, Img2),
             size_2d(Img2, W, H),
             circle_points_2d(Cen, Rad, [W, H], C_pts),
             draw_points_2d(Img2, C_pts, red),
             draw_points_2d(Img2, Pts, blue),
             atom_number(Name, ImgID),
             %showimg_win(Img2, Name),
             atomic_concat('../../tmp/pics/', Name, PicName),
             atomic_concat(PicName, '.png', PicPath),
             save_img(Img2, PicPath),
             release_img(Img2)
         );
     true
    ), !,
    %% debug end
    release_img(Img),
    release_sp(SP).

sps_box_gng_pts(Img, SP, IDs, Return):-
    sps_box_sample_rand_line(SP, IDs, Pts),
    pts_color_2d(Img, Pts, LABs),
    color_change_points(green, not(green), Pts, LABs, R1),
    color_change_points(not(green), green, Pts, LABs, R2),
    append(R1, R2, Return).

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

