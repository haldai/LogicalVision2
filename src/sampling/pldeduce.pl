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
/* Deduces fact by sampling
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

:- ensure_loaded(['../sampling/plsampling.pl']).

/* line sampling to deduce light source */
sample_light_source_dir(Imgseq, Frame, Pt, Dir):-
    size_2d(Imgseq, W, H),
    random(0, W, X), random(0, H, Y), % random position
    radial_lines_2d([X, Y, Frame], 0, 360, 2, Lines), % sample radial lines
    sample_lines_L_grads(Imgseq, Lines, Pts, Gs),
    % TODO statistics of positve and negative gradients
    grad_prop(Pts, Gs, Prp),
    max_list_idx(Prp, Max_idx),
    nth1(Max_idx, Lines, Max_line),
    Max_line = [Pt, Dir].

/* proportion of gradients (Pos/Neg) */
grad_prop([], [], []):-
    !.
grad_prop([Pts | Points], [Grd | Grads], [P | Props]):-
    items_key_geq_T(Pts, Grd, 2, Pos), % grad+ (>=2)
    items_key_less_T(Pts, Grd, -1, Neg), % grad- (< -1)
    length(Pos, NPos), length(Neg, NNeg),
    NPos + NNeg > 20, % no trivial directions (only has few brightness changes)
    P is NPos/(NNeg + 10e-10),
    grad_prop(Points, Grads, Props), !.
grad_prop([_ | Points], [_ | Grads], [P | Props]):-
    P is -1,
    grad_prop(Points, Grads, Props).
    

