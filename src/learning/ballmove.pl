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
%%%%%%%%%%%%%%%%%%%%%%%%%
% learn kicking
%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module('metagol').

% background knowledge for learning ball movement

% target hypothesis
% ball_move(Dir, Frame):-
%     predecessor(Frame, Frame1)
%     ball_move(Dir, Frame1).
% ball_move(Dir, Frame):-
%     kick_ball(Dir, Frame).



%=========================
% examples for learning
%=========================
pos([kickball([-7, -9], 94),
     kickball([-175, 3], 368),
     kickball([0, -1], 499),
     kickball([11, 4], 589),
     kickball([1, 0], 731),
     kickball([76, -1], 855),
     kickball([0, 1], 937)]).

neg([]).
