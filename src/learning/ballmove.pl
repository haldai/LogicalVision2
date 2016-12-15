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
