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
/* Prolog sampling
 *     Sample ellipses on *image*.
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

:- ensure_loaded(['../sampling/plsampling.pl']).

/* Sample one ellipse in image
 * @Img: input image
 * @Elps: parameter of the sampled ellipse, Elps = [Center, [A, B, ALPHA]],
 *     Center is the center of the ellipse
 *     A, B are axis length
 *     ALPHA: tilt angle
 */
sample_ellipse(Img, Elps):-
    % TODO:
    Elps = [Center, [A, B, ALPHA]],
    fail.

/* Abuductive definition of an object
 */
object(X):-
    ellipse(X, Elps), % abduce an ellipse, Elps = [Center, Param].
    fail.

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

