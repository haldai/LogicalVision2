/* Sampling module
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */
:- load_foreign_library(foreign('../../libs/cvsampler.so')),
   load_foreign_library(foreign('../../libs/cvcluster.so')).

:- ensure_loaded(['../utils/utils.pl']),
   ensure_loaded(['../sampling/plregion.pl']).

%======================================
% line with angle to start-end vector
%======================================
% given a desired 2d angle (DEG) of a line,
%   get its direction vector
angle2dir_2d(Angle, Dir):-
    Angle_rad = Angle * pi/180,
    X is round(10e6 * cos(Angle_rad)),
    Y is round(10e6 * sin(Angle_rad)),
    Dir = [X, Y, 0].

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
    line_seg_points(Start, End, [W, H, D], Points),
    pts_var(Imgseq, Points, Vars).

% sample_line_scharr(+Imgseq, +Start, +Direct, -Points, -Grads)
% sample a line to get its points and cooresponding scharr gradients
sample_line_scharr(Imgseq, Start, Direct, Points, Grads):-
    size_3d(Imgseq, W, H, D),
    line_points(Start, Direct, [W, H, D], Points),
    pts_scharr(Imgseq, Points, Grads).

% sample_line_seg_scharr(+Imgseq, +Start, +End, -Points, -Grads)
% sample a line segment to get its points and cooresponding variance
sample_line_seg_scharr(Imgseq, Start, End, Points, Grads):-
    size_3d(Imgseq, W, H, D),
    line_seg_points(Start, End, [W, H, D], Points),
    pts_scharr(Imgseq, Points, Grads).

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
% sample a line segment to get its points and cooresponding brightness
sample_line_seg_color_L(Imgseq, Start, End, Points, Lchannel):-
    size_3d(Imgseq, W, H, D),
    line_points(Start, End, [W, H, D], Points),
    pts_color(Imgseq, Points, Colors),
    column(1, Colors, Lchannel).

%====================
% sample gradients
%====================
% sample_line_L_grad(+Imgseq, +Pt, +Dir, -Points, -Grads)
% sample a line and get its brightness gradients
sample_line_L_grad(Imgseq, Pt, Dir, Points, Grads):-
    sample_line_color(Imgseq, Pt, Dir, Points, Colors),
    grad_l(Colors, Grads).

sample_lines_L_grads(_, [], [], []):-
    !.
sample_lines_L_grads(Imgseq, [L | Lines], [Pts | Points], [Grds | Grads]):-
    L = [Pt, Dir],
    sample_line_color(Imgseq, Pt, Dir, Pts, Colors),
    grad_l(Colors, Grds),
    sample_lines_L_grads(Imgseq, Lines, Points, Grads).


% sample_line_seg_L_grad(+Imgseq, +Start, +End, -Points, -Grads)
% sample a line segment and get its brightness gradients
sample_line_seg_L_grad(Imgseq, Start, End, Points, Grads):-
    sample_line_seg_color(Imgseq, Start, End, Points, Colors),
    grad_l(Colors, Grads).

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

%=================================================================
% get items whose keys are greater/less than threshold,
%     i.e. the map of points and gradients (G-[X, Y, Z]).
%=================================================================
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

%=================================================================
% sort a mapping
%     i.e. the map of points and gradients (G-[X, Y, Z]).
%=================================================================
mapsort(Keys, Values, S_Keys, S_Values):-
    pairs_keys_values(Pairs, Keys, Values),
    keysort(Pairs, S_Pairs),
    pairs_keys_values(S_Pairs, S_Keys, S_Values).

%===========================================================================
% Sample many radial lines.
%   The sampled lines cross same Point with degree from Start_deg to
%   End_deg, Step is the stepsize. Lines = [[Start_point1, End_point1], ...]
%===========================================================================
radial_lines_2d(Point, Start_deg, End_deg, Step, Rays):-
    Step >= 1,
    my_findall(Ray,
               radial_line_2d(Point, Start_deg, End_deg, Step, Ray),
               Rays).
radial_line_2d(Point, Start_deg, End_deg, Step, Ray):-
    between(Start_deg, End_deg, Ang),
    Ang mod Step =:= 0,
    angle2dir_2d(Ang, Dir),
    Ray = [Point, Dir].

%=================================================================
% sample line segments and cluster them according to color hists
%=================================================================
% sample line get segments and histograms:
sample_line_hists(Imgseq, [Point, Dir], Thresh_Scharr, Seg_Hist_Pairs):-
    % get edge points
    line_pts_scharr_geq_T(Imgseq, Point, Dir, Thresh_Scharr, Edge_pts),
    seg_hists_pairs(Imgseq, Edge_pts, Seg_Hist_Pairs).
% sample linesegment and get smaller segments and histograms
sample_seg_hists(Imgseq, [Start, End], Thresh_Scharr, Seg_Hist_Pairs):-
    line_seg_pts_scharr_geq_T(Imgseq, Start, End, Thresh_Scharr, Edge_pts),
    seg_hists_pairs(Imgseq, Edge_pts, Seg_Hist_Pairs).
% get line segment hists and return Seg-Hist pairs
seg_hists_pairs(_, [_], []):-
    !.
seg_hists_pairs(Imgseq, [P1, P2 | Ps], [Seg-Hist | SHs]):-
    Seg = [P1, P2],
    size_3d(Imgseq, W, H, D),
    line_seg_points(P1, P2, [W, H, D], Pts),
    points_color_hist(Imgseq, Pts, Hist),
    seg_hists_pairs(Imgseq, [P2 | Ps], SHs).

% multiple lines
sample_lines_hists(Imgseq, [L | Lines],
                   Thresh_Scharr, Re):-
    sample_lines_hists(Imgseq, [L | Lines], Thresh_Scharr, Re, []).
sample_lines_hists(_, [], _, Re, Re):-
    !.
sample_lines_hists(Imgseq, [L | Lines],
                   Thresh_Scharr, Re, Temp):-
    sample_line_hists(Imgseq, L, Thresh_Scharr, SH),
    append(SH, Temp, Temp1),
    sample_lines_hists(Imgseq, Lines, Thresh_Scharr, Re, Temp1).
% multiple segs
sample_line_segs_hists(Imgseq, [L | Lines],
                   Thresh_Scharr, Re):-
    sample_line_segs_hists(Imgseq, [L | Lines], Thresh_Scharr, Re, []).
sample_line_segs_hists(_, [], _, Re, Re):-
    !.
sample_line_segs_hists(Imgseq, [L | Lines],
                   Thresh_Scharr, Re, Temp):-
    sample_line_seg_hists(Imgseq, L, Thresh_Scharr, SH),
    append(SH, Temp, Temp1),
    sample_line_segs_hists(Imgseq, Lines, Thresh_Scharr, Re, Temp1).

% clustering the segments
cluster_seg_hist_pairs(SHs, Num_Clusters, Signed_SHs):-
    pairs_keys_values(SHs, _, Hists),
    kmeans(Hists, Num_Clusters, _, Assign),
    pairs_keys_values(Signed_SHs, SHs, Assign).

