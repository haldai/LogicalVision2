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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prolog color definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-ensure_loaded(['../utils/utils.pl',
                 '../utils/geometry.pl']).

%====================
% color definitions
%====================
color([_, A_, B_], green):-
    A is A_ - 128, B is B_ - 128,
    A =< -10, B >= 10.
color([L_, A_, B_], white):-
    L is L_*100/255,
    A is A_ - 128, B is B_ - 128,
    R is sqrt(A**2 + B**2),
    R =< 45,
    L >= 60.
color([L_, A_, B_], black):-
    L is L_*100/255,
    A is A_ - 128, B is B_ - 128,
    R is sqrt(A**2 + B**2),
    R =< 45,
    L =< 40.
color([L_, A_, B_], gray):-
    L is L_*100/255,
    A is A_ - 128, B is B_ - 128,
    R is sqrt(A**2 + B**2),
    R =< 45,
    L > 40,
    L < 60.
color(C, not(Color)):-
    ground(Color),
    not(color(C, Color)), !.
color(C, only(Color)):-
    ground(Color),
    findall(C_, color(C, C_), Cls),
    Cls = [Color], !.
all_colors(LAB, Colors):-
    findall(C, color(LAB, C), Colors).

%===================
% other operations
%===================
colors([], []):-
    !.
colors([LAB | LABs], [C | Cs]):-
    findall(C_, color(LAB, C_), C),
    colors(LABs, Cs), !.

% get colors of list of points lists from image
% get_colors_list_2d(Img, [List1, List2, ...], [Colors1, Colors2, ...]).
get_colors_list_2d(_, [], []):-
    !.
get_colors_list_2d(Img, [S | Segs], [C | Cs]):-
    %print_list(S),    
    pts_color_2d(Img, S, LABs),
    colors(LABs, C),
    get_colors_list_2d(Img, Segs, Cs).

%======================
% color changing points
%======================
% color_change_point(Color1, Color2, Point_list, LAB_values, Returned_Points)
% return points changing from color1 to color2
color_change_points(_, _, [_], [_], []):-
    !.
color_change_points(Color1, Color2, [_, P2 | Ps], [C1, C2 | Cs], Qs):-
    color(C1, Color1), color(C2, Color1), not(color(C2, Color2)),
    color_change_points(Color1, Color2, [P2 | Ps], [C2 | Cs], Qs), !.
color_change_points(Color1, Color2, [_, P2 | Ps], [C1, C2 | Cs], Qs):-
    color(C1, Color1), color(C2, Color1),
    color(C1, Color2), color(C2, Color2), 
    color_change_points(Color1, Color2, [P2 | Ps], [C2 | Cs], Qs), !.
color_change_points(Color1, Color2, [_, P2 | Ps], [C1, C2 | Cs], [P2 | Qs]):-
    color(C1, Color1), color(C2, Color2),
    color_change_points(Color1, Color2, [P2 | Ps], [C2 | Cs], Qs), !.
color_change_points(Color1, Color2, [_, P2, P3| Ps],
                   [C1, _, C3 | Cs], [P2 | Qs]):-
    color(C1, Color1), color(C3, Color2),
    color_change_points(Color1, Color2, [P3 | Ps], [C3 | Cs], Qs), !.
color_change_points(Color1, Color2, [_, _, P3, P4| Ps],
                   [C1, _, _, C4 | Cs], [P3 | Qs]):-
    color(C1, Color1), color(C4, Color2),
    color_change_points(Color1, Color2, [P4 | Ps], [C4 | Cs], Qs), !.
color_change_points(Color1, Color2, [_, _, P3, _, P5 | Ps],
                   [C1, _, _, _, C5 | Cs], [P3 | Qs]):-
    color(C1, Color1), color(C5, Color2),
    color_change_points(Color1, Color2, [P5 | Ps], [C5 | Cs], Qs), !.
color_change_points(Color1, Color2, [_, P2 | Ps], [_, C2 | Cs], Qs):-
    color_change_points(Color1, Color2, [P2 | Ps], [C2 | Cs], Qs), !.

% determine color change point by index
color_chg_idx(Img, Pts, I, Color1, Color2):-
    I1 is I - 1,
    nth1(I1, Pts, P1), nth1(I, Pts, P2),
    point_color_2d(Img, P1, C1), point_color_2d(Img, P2, C2),
    color(C1, Color1), color(C2, Color1), not(color(C2, Color2)),
    !,
    fail.
color_chg_idx(Img, Pts, I, Color1, Color2):-
    I1 is I - 1,
    nth1(I1, Pts, P1), nth1(I, Pts, P2),
    point_color_2d(Img, P1, C1), point_color_2d(Img, P2, C2),
    color(C1, Color1), color(C2, Color1),
    color(C1, Color2), color(C2, Color2),
    !,
    fail.
color_chg_idx(Img, Pts, I, Color1, Color2):-
    I1 is I - 1,
    nth1(I1, Pts, P1), nth1(I, Pts, P2),
    point_color_2d(Img, P1, C1), point_color_2d(Img, P2, C2),
    color(C1, Color1), color(C2, Color2),
    !.
color_chg_idx(Img, Pts, I, Color1, Color2):-
    I1 is I - 1, I3 is I + 1,
    nth1(I1, Pts, P1), nth1(I3, Pts, P3),
    point_color_2d(Img, P1, C1), point_color_2d(Img, P3, C3),
    color(C1, Color1), color(C3, Color2),
    !.
color_chg_idx(Img, Pts, I, Color1, Color2):-
    I1 is I - 1, I4 is I + 2,
    nth1(I1, Pts, P1), nth1(I4, Pts, P4),
    point_color_2d(Img, P1, C1), point_color_2d(Img, P4, C4),
    color(C1, Color1), color(C4, Color2),
    !.
color_chg_idx(Img, Pts, I, Color1, Color2):-
    I1 is I - 2, I5 is I + 2,
    nth1(I1, Pts, P1), nth1(I5, Pts, P5),
    point_color_2d(Img, P1, C1), point_color_2d(Img, P5, C5),
    color(C1, Color1), color(C5, Color2),
    !.
