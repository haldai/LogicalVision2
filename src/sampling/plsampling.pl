/* Sampling module
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */
:- load_foreign_library(foreign('../../libs/cvsampler.so')).

:- ['../utils/utils.pl'].

%===========================
% sample variance of a line
%===========================
% sample_line_var(+Imgseq, +Start, +Direct, -Points, -Vars)
% sample a line to get its points and cooresponding variance
sample_line_var(Imgseq, Start, Direct, Points, Vars):-
    size_3d(Imgseq, W, H, D),
    line_points(Start, Direct, [W, H, D], Points),
    pts_var(Imgseq, Points, Vars).

% sample_line_seg_var(+Imgseq, +Start, +End, -Points, -Vars)
% sample a line segment to get its points and cooresponding variance
sample_line_seg_var(Imgseq, Start, End, Points, Vars):-
    size_3d(Imgseq, W, H, D),
    line_points(Start, End, [W, H, D], Points),
    pts_var(Imgseq, Points, Vars).

%===========================
% sample color of a line
%===========================
% sample_line_color(+Imgseq, +Start, +Direct, -Points, -Colors)
% sample a line to get its points and cooresponding color
sample_line_color(Imgseq, Start, Direct, Points, Colors):-
    size_3d(Imgseq, W, H, D),
    line_points(Start, Direct, [W, H, D], Points),
    pts_color(Imgseq, Points, Colors).

% sample_line_seg_color(+Imgseq, +Start, +End, -Points, -Colors)
% sample a line segment to get its points and cooresponding color
sample_line_seg_color(Imgseq, Start, End, Points, Colors):-
    size_3d(Imgseq, W, H, D),
    line_points(Start, End, [W, H, D], Points),
    pts_color(Imgseq, Points, Colors).

% sample_line_color_L(+Imgseq, +Start, +Direct, -Points, -Lchannel)
% sample a line to get its points and cooresponding brightness
sample_line_color_L(Imgseq, Start, Direct, Points, Lchannel):-
    size_3d(Imgseq, W, H, D),
    line_points(Start, Direct, [W, H, D], Points),
    pts_color(Imgseq, Points, Colors),
    column(1, Colors, Lchannel). % nth1 starts with index 1

% sample_line_seg_color_L(+Imgseq, +Start, +End, -Points, -Lchannel)
% sample a line segment to get its points and cooresponding variance
sample_line_seg_color_L(Imgseq, Start, End, Points, Lchannel):-
    size_3d(Imgseq, W, H, D),
    line_points(Start, End, [W, H, D], Points),
    pts_color(Imgseq, Points, Colors),
    column(0, Colors, Lchannel).

%================================
% 1d Gradients for sampled lines
%================================
% grad_l/a/b(+List_of_LAB, -Grad)
% given a set of LAB colors, return the gradient of brightness channel
grad_l(List_of_LAB, Grad):-
    grad_column(1, List_of_LAB, Grad).
grad_a(List_of_LAB, Grad):-
    grad_column(2, List_of_LAB, Grad).
grad_b(List_of_LAB, Grad):-
    grad_column(3, List_of_LAB, Grad).

% grad(+List, -Grad)
% compute gradient of a list of numbers, the first gradient is always 0
% e.g. grad([1, 2, 3, 5, 7], [0, 1, 1, 2, 2]).
grad(List, Grad):-
    grad(List, Grad, start).
grad([], [], _).
grad([L | Ls], [G | Gs], start):-
    G = 0,
    grad(Ls, Gs, L), !.
grad([L | Ls], [G | Gs], P):-
    G is L - P,
    grad(Ls, Gs, L).

% grad_column(+Column, +List_of_LAB, -Grad)
grad_column(Col, List_of_Vec, Grad):-
    column(Col, List_of_Vec, Column),
    grad(Column, Grad).

%==============================================================
% get items whose keys are greater/less than threshold
%==============================================================
items_key_geq_T(Items, Keys, Thresh, Return):-
    idx_element_geq_T(Keys, Thresh, Indices),
    index_select(Indices, Items, Return).

items_key_less_T(Items, Keys, Thresh, Return):-
    idx_element_less_T(Keys, Thresh, Indices),
    index_select(Indices, Items, Return).

% element_idx_geq_T(+List, +Thresh, -Indices)
idx_element_geq_T(List, Thresh, Indices):-
    idx_element_geq_T(List, Thresh, Indices, 1).

% index start from 1
idx_element_geq_T([], _, [], _):-
    !.
idx_element_geq_T([E | Es], Thresh, [N | Ns], N):-
    E >= Thresh,
    N1 is N + 1,
    idx_element_geq_T(Es, Thresh, Ns, N1), !.
idx_element_geq_T([E | Es], Thresh, Ns, N):-
    E < Thresh,
    N1 is N + 1,
    idx_element_geq_T(Es, Thresh, Ns, N1), !.

% element_idx_geq_T(+List, +Thresh, -Indices)
idx_element_less_T(List, Thresh, Indices):-
    idx_element_less_T(List, Thresh, Indices, 1), !.

% index start from 1
idx_element_less_T([], _, [], _):-
    !.
idx_element_less_T([E | Es], Thresh, [N | Ns], N):-
    E < Thresh,
    N1 is N + 1,
    idx_element_less_T(Es, Thresh, Ns, N1), !.
idx_element_less_T([E | Es], Thresh, Ns, N):-
    E >= Thresh,
    N1 is N + 1,
    idx_element_less_T(Es, Thresh, Ns, N1), !.
