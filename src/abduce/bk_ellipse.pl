/* Background knowledge of ellipse
 */

% 1. sample edge points in image (line sampling)
% 2. hypothesis: fit ellipses with edge points
% 3. validation: ellipse definition
%      if not valid, do more sampling
%      if valid, add to bk? or use statistics?

:- ensure_loaded(['../sampling/plsampling.pl']).

/* ellipse(+Img, +Param, +VAR_THRESH, +P_THRESH)
 * Definition of ellipse:
 * @Param = [[X, Y, F], [A, B, ALPHA]] represents parameters of
 *   the ellipse. (X, Y, F) is the center of ellipse, x and y are coordinates,
 *   F is the frame number. A, B are axis length, ALPHA is tilt angle
 * @VAR_THRESH:
 * @P_THRESH:
 */
ellipse(Imgseq, [[X, Y, F], [A, B, ALPHA]], VAR_THRESH, P_THRESH):-
    ellipse(Imgseq, [[X, Y, F], [A, B, ALPHA]], VAR_THRESH, P_THRESH, _, _).
ellipse(Imgseq, [[X, Y, F], [A, B, ALPHA]], VAR_THRESH, P_THRESH, Pos, PTS):-
    size_3d(Imgseq, W, H, D),
    ellipse_points([X, Y, F], [A, B, ALPHA], [W, H, D], PTS),
    pts_scharr(Imgseq, PTS, VARS),
    write(VARS), nl,
    items_key_geq_T(PTS, VARS, VAR_THRESH, Pos),
    length(Pos, N_Pos), length(PTS, Total),
    R is N_Pos / Total,
    write(R), nl,
    (R >= P_THRESH; true), !.
