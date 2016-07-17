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
    

