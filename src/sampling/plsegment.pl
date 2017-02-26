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
:- ensure_loaded('../utils/utils.pl').

% number of random lines for determine background and non-background colors
rand_line_num(500).
num_clusters(2).
% number of random line segments to be sampled 
rand_seg_num(5).
% threshold of scharr gradient
thresh_scharr(2.0).
% thresholds of cross segment sampling
cross_num(4). % number of crossed segs to be sampled
cross_eval_thresh(0.6).
% recursively eval turns
rec_eval_turn(3).

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

% multiple line segs
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
cluster_seg_hist_pairs(SHs, Num_Clusters, Signed_SHs, Centroids):-
    pairs_keys_values(SHs, _, Hists),
    kmeans(Hists, Num_Clusters, _, Assign, Centroids),
    pairs_keys_values(Signed_SHs, SHs, Assign).

% given centroids and sampled lines, return non background segments.
get_non_bg_segs(_, _, [], []):-
    !.
get_non_bg_segs(Imgseq, Centroids, [[Point, Dir] | Lines], Segs):-
    sample_line_get_seg_classes(Imgseq, Centroids, Point, Dir, Seg_Classes),
    findall(Seg, (member(Seg-C, Seg_Classes), C =\= 0), NonBGs),
    get_non_bg_segs(Imgseq, Centroids, Lines, Segs1),
    append(NonBGs, Segs1, Segs).

%==========================
% find the longest segment
%==========================
longest_seg(Segs, Re):-
    longest_seg(Segs, [[0, 0], [0, 0]], Re).
longest_seg([], Re, Re).
longest_seg([[S, E] | Ss], [Ts, Te], Re):-
    eu_dist(S, E, L1), eu_dist(Ts, Te, L2),
    L1 >= L2,
    longest_seg(Ss, [S, E], Re), !.
longest_seg([[S, E] | Ss], [Ts, Te], Re):-
    eu_dist(S, E, L1), eu_dist(Ts, Te, L2),
    L1 < L2,
    longest_seg(Ss, [Ts, Te], Re), !.
    
%===================================================================
% initilization: sample the color of background and non-backgrounds
%===================================================================
color_bg_nonbg(Imgseq, Frame, Centroids, NonBG_Segs):-
    write('sampling random lines.'), nl,
    rand_line_num(N),
    size_3d(Imgseq, W, H, _),
    rand_sample_2d_lines(Frame, [W, H], N, Lines),
    thresh_scharr(T),
    sample_lines_hists(Imgseq, Lines, T, SHs),
    num_clusters(Num_Clusters),
    get_centroids(SHs, Num_Clusters, Centroids, NonBG_Segs),
    !.

get_centroids(SHs, Num_Clusters, [BG_Centroid, NonBG_Centroids], NonBG_Segs):-
    cluster_seg_hist_pairs(SHs, Num_Clusters, SHs_Sign, Cents),
    write('clustering finished.'), nl,
    transpose_pairs(SHs_Sign, Sign_SHs),
    % group the clusters, the larger one is BG
    group_pairs_by_numbers(Sign_SHs, 4, Grps),
    Grps = [4-G4, 3-G3, 2-G2, 1-G1, 0-G0],
    % the group with maximum local length is background
    total_seg_length(G0, L0), total_seg_length(G1, L1),
    total_seg_length(G2, L2), total_seg_length(G3, L3),
    total_seg_length(G4, L4),
    write(L0), write(', '), write(L1), write(', '),
    write(L2), write(', '), write(L3), write(', '), write(L4), nl,
    max_list([L0, L1, L2, L3, L4], Max),
    nth0(Idx, [L0, L1, L2, L3, L4], Max), % get largest cluster's index
    nth0(Idx, Cents, BG_Centroid, NonBG_Centroids), % set it as BG centorid
    switch(Idx, [0: append([G1, G2, G3, G4], NBGs),
                 1: append([G0, G2, G3, G4], NBGs),
                 2: append([G0, G1, G3, G4], NBGs),
                 3: append([G0, G1, G2, G4], NBGs),
                 4: append([G0, G1, G2, G3], NBGs)
                ]
          ),
    pairs_keys(NBGs, NonBG_Segs).

%========================================================
% determine the class of a line segment 
%========================================================
%seg_is_bg(Imgseq, [P1, P2], [BG_Centroid, NonBG_Centroids]):-
%    size_3d(Imgseq, W, H, D),
%    line_seg_points(P1, P2, [W, H, D], Pts),
%    points_color_hist(Imgseq, Pts, Hist),
%    eu_dist(Hist, BG, D1),
%    eu_dist(Hist, NonBG, D2),
%    D1 >= D2.

class_of_seg(Imgseq, Seg, [BG_Centroid, NonBG_Centroids], Class):-
    append([BG_Centroid], NonBG_Centroids, Cents),
    length(Cents, L),
    L1 is L - 1,
    class_of_seg(Imgseq, Seg, Cents, Class, -1, 10e5, L1).

class_of_seg(_, _, _, Class, Class, _, -1):-
    !.
class_of_seg(Imgseq, Seg, Cents, Class, Temp_Class, Temp_dist, L):-
    nth0(L, Cents, Centroid),
    seg_hist(Imgseq, Seg, Hist),
    eu_dist(Hist, Centroid, Dist),
    (Dist =< Temp_dist ->
         (Temp_dist1 = Dist,
          Temp_Class1 = L,
          !);
     (Temp_dist1 = Temp_dist,
      Temp_Class1 = Temp_Class,
     !)
    ),
    L1 is L - 1,
    class_of_seg(Imgseq, Seg, Cents, Class, Temp_Class1, Temp_dist1, L1).

% get hist feature of a line segment
seg_hist(Imgseq, [P1, P2], Hist):-
    size_3d(Imgseq, W, H, D),
    line_seg_points(P1, P2, [W, H, D], Pts),
    points_color_hist(Imgseq, Pts, Hist).

% input a list of edge points, get segments and get each classes
% Cents = [BG, [NonBG1, NonBG2, NonBG3]].
class_of_segs(Imgseq, [P1, P2 | Pts], Cents, Re):-
    class_of_segs(Imgseq, [P1, P2 | Pts], Cents, [], Re).
class_of_segs(_, [_], _, Re, Re):-
    !.
class_of_segs(Imgseq, [P1, P2 | Pts], Cents, [], Re):-
    class_of_seg(Imgseq, [P1, P2], Cents, Class),
    class_of_segs(Imgseq, [P2 | Pts], Cents, [[P1, P2]-Class], Re),
    !.
class_of_segs(Imgseq, [P1, P2 | Pts], Cents, [LS-LC | SCs], Re):-
    Seg0 = [P1, P2],
    class_of_seg(Imgseq, [P1, P2], Cents, Class),
    % if this segment has same class with last seg, merge them
    (Class == LC ->
         (LS = [P0, P1],
          Seg = [P0, P2],
          class_of_segs(Imgseq, [P2 | Pts], Cents, [Seg-Class | SCs], Re),
          !
         );
     (Seg = Seg0,
      class_of_segs(Imgseq, [P2 | Pts], Cents, [Seg-Class, LS-LC | SCs], Re),
      !)
    ).

%============================================
% sample a new line and get segment classes
%============================================
sample_line_get_seg_classes(Imgseq, Centroids, Point, Dir, Seg_Classes):-
    thresh_scharr(Thresh_Scharr),
    line_pts_scharr_geq_T(Imgseq, Point, Dir, Thresh_Scharr, Edge_pts),
    class_of_segs(Imgseq, Edge_pts, Centroids, Seg_Classes).

%======================================
% sample lines that crosses a segment,
%   [[X1, Y1, Z1], [X2, Y2, Z2]],
%   seg must on a frame, i.e. Z1 == Z2
%======================================
sample_rand_lines_cross_seg_2d(_, N, []):-
    N =< 0, !.
sample_rand_lines_cross_seg_2d(Seg, N, [[Pt, Dir] | Lines]):-
    N > 0,
    Seg = [[_, _, Z], [_, _, Z]],
    rand_point_on_seg(Seg, Pt),
    rand_2d_angle_vec([X, Y]), Dir = [X, Y, 0],
    N1 is N - 1,
    sample_rand_lines_cross_seg_2d(Seg, N1, Lines).

sample_grid_lines_cross_seg_2d(Seg, Total, [[Pt, Dir] | Lines]):-
    N is Total - 1,
    sample_grid_lines_cross_seg_2d(Seg, N, Total, [[Pt, Dir] | Lines]).
sample_grid_lines_cross_seg_2d(_, N, _, []):-
    N =< 0, !.
sample_grid_lines_cross_seg_2d(Seg, N, Total, [[Pt, Dir] | Lines]):-
    N > 0,
    Seg = [[X1, Y1, Z], [X2, Y2, Z]],
    % rand_point_on_seg(Seg, Pt),
    section_on_seg(Seg, N, Total, Pt),
    Seg_Dir_X is X2 - X1, Seg_Dir_Y is Y2 - Y1,
    ((Seg_Dir_Y =\= 0, X is 1, Y is -Seg_Dir_X / Seg_Dir_Y);
     (Seg_Dir_Y == 0, X is 0, Y is 1)),
    !,
    Dir = [X, Y, 0],
    N1 is N - 1,
    sample_grid_lines_cross_seg_2d(Seg, N1, Total, Lines).

%===============================================
% sample line segment that cross a given point
%===============================================
random_line_segs_crossing_point(Imgseq, Point, Bound, Segs):-
    rand_seg_num(N),
    random_line_segs_crossing_point(Imgseq, Point, Bound, N, Segs).

random_line_segs_crossing_point(_, _, _, 0, []):-
    !.
random_line_segs_crossing_point(Imgseq, Point, Bound, N, [S | Segs]):-
    rand_2d_angle_vec([X, Y]),
    thresh_scharr(Th),
    line_pts_scharr_geq_T(Imgseq, Point, [X, Y, 0], Th, Edg_pts),
    pts2segs(Edg_pts, Ss),
    box_contain_point(Ss, Point, S),
    N1 is N - 1,
    random_line_segs_crossing_point(Imgseq, Point, Bound, N1, Segs).

box_contain_point([], _, []):-
    !.
box_contain_point([[Start, End] | _], Point, S):-
    in_box([Start, End], Point),
    S = [Start, End], !.
box_contain_point([_ | Ss], Point, S):-
    box_contain_point(Ss, Point, S).

%===========================================================================
% crossed segment classes:
%   given a Seg-Class and a set of lines, find the intersected
%   segments of the lines with Seg, and return if their classes are "Class"
%===========================================================================
cross_seg_classes(_, _, _, [], []):-
    !.
cross_seg_classes(Imgseq, Seg, Centroids, [[P, Dir] | Ls], [CLS | Cs]):-
    sample_line_get_seg_classes(Imgseq, Centroids, P, Dir, Seg_Cls),
    % P here is a point belongs to Seg, so only make interval tests!
    ((crossed_seg(P, Seg_Cls, [CSeg-C]), % P is not an edge point
      CLS = CSeg-C,
      cross_seg_classes(Imgseq, Seg, Centroids, Ls, Cs)
     );
     (crossed_seg(P, Seg_Cls, [CSeg0-C0, CSeg1-C1]), % P is an edge point
      CLS = CSeg0-C0,
      Cs = [CSeg1-C1 | Cs1],
      cross_seg_classes(Imgseq, Seg, Centroids, Ls, Cs1)
     )), !.

% use starting point of line [P, D] to find intersected segments
%   by interval test
crossed_seg(_, [], []):-
    !.
crossed_seg([X, Y, Z], [S-C | Ss], [S-C | CSs]):-
    S = [[X1, Y1, Z], [X2, Y2, Z]],
    ((between(X1, X2, X); between(X2, X1, X)),
     (between(Y1, Y2, Y); between(Y2, Y1, Y))), !,
    crossed_seg([X, Y, Z], Ss, CSs).
crossed_seg(P, [_ | Ss], CSs):-
    crossed_seg(P, Ss, CSs).

% remove segments duplicates with ellipse
remove_duplicate_segs([], _, _, []):-
    !.
remove_duplicate_segs([S | Seg], [Cen, Para], Bound, Ss):-
    intsct_seg_elps(S, [Cen, Para], Bound, S1),
    seg_length(S, Ls), seg_length(S1, Ls1),
    P is Ls1/Ls,
    P >= 0.2,
    remove_duplicate_segs(Seg, [Cen, Para], Bound, Ss), !.
remove_duplicate_segs([S | Seg], Elps, Bound, [S | Ss]):-
    remove_duplicate_segs(Seg, Elps, Bound, Ss), !.
    
%===================================
% make conjectures
%===================================
% discover object in a Frame of ImageSequence
%discover_object(_, _, _, [], []):-
%    !.
%% discover object in a Frame of ImageSequence
%discover_object(Imgseq, Frm, Centroids, [S | Segs],
%                [elps(Cen, Para, C) | Elpses]):-
%    rec_eval_turn(Turn),
%    class_of_seg(Imgseq, S, Centroids, C),
%    length(Segs, L), write('VSegs #'), write(L), nl,
%    eval_segs(Imgseq, Frm, [S-C], Centroids, Turn, _, VSegs1),
%    %write(eval_segs(Imgseq, Frm, [S-C], Centroids, Turn, _, VSegs1)), nl,
%    VSegs1 \= [[]],
%    VSegs1 = [VSegs],
%    print_list(VSegs),
%    append(VSegs, Edg_Pts_1), list_to_set(Edg_Pts_1, Edg_Pts),
%    length(Edg_Pts, LE), LE >= 5,
%    fit_elps(Edg_Pts, Cen, Para),
%    write('fitted parameters: '), write(Cen), write(', '), write(Para), nl,    
%    size_3d(Imgseq, W, H, D),
%    remove_duplicate_segs(Segs, [Cen, Para], [W, H, D], Segs1),
%    %Segs1 = Segs,
%
%    % DEBUG START
%    % write('Current Seg: '), write(S-C), nl,    
%    % write('Crossed Segs: '), nl, print_list_ln(Crossed_SCs),
%    size_3d(Imgseq, W, H, D),
%    seq_img(Imgseq, Frm, IMG1),
%    clone_img(IMG1, IMG2),
%    draw_line_segs_2d(IMG2, VSegs, red),
%    ellipse_points(Cen, Para, [W, H, D], Elps),
%    draw_points_2d(IMG2, Elps, green),
%    showimg_win(IMG2, 'debug'),
%%    release_img(IMG2),
%    % DEBUG END
%
%    discover_object(Imgseq, Frm, Centroids, Segs1, Elpses), !.
%discover_object(Imgseq, Frm, Centroids, [_ | Segs], Elps):-
%    discover_object(Imgseq, Frm, Centroids, Segs, Elps), !.
%
%% average ellipse (according to parameters) of a list of ellipses
%average_elps(Elpses, elps(Cen, Param, Color)):-
%    findall(Center, member(elps(Center, _, _), Elpses), Centers),
%    findall(A, member(elps(_, [A, _, _], _), Elpses), As),
%    findall(B, member(elps(_, [_, B, _], _), Elpses), Bs),
%    findall(C, member(elps(_, [_, _, C], _), Elpses), Cs),
%    findall(Col, member(elps(_, _, Col), Elpses), Colors),
%    average(Centers, Cen),    
%    average(As, Av), average(Bs, Bv), average(Cs, Cv),
%    Param = [Av, Bv, Cv],
%    mode(Colors, Color), !.
%    
%
%%%%%%%%%% segment-wise evaluation %%%%%%%%%
%eval_segs(_, _, [], _, _, [], []):-
%    !.
%% class BG
%eval_segs(Imgseq, Frm, [_-0 | Css], Cents, N, [0 | Res], VSs):-
%    eval_segs(Imgseq, Frm, Css, Cents, N, Res, VSs),
%    !.
%% level-0: success
%eval_segs(Imgseq, Frm, [S-C | Css], Cents, 0, [1 | Res], VSs):-
%    C =\= 0,
%    cross_num(TL), sample_grid_lines_cross_seg_2d(S, TL, Ls),
%    cross_seg_classes(Imgseq, S, Cents, Ls, Crossed_SCs),
%    findall(Seg, member(Seg-C, Crossed_SCs), NBGs),
%    length(NBGs, Lnbg), length(Crossed_SCs, Total),
%    Prob is Lnbg/Total,
%    cross_eval_thresh(TE),
%    Prob >= TE,
%    eval_segs(Imgseq, Frm, Css, Cents, 0, Res, VSs), !.
%% level-0: failed
%eval_segs(Imgseq, Frm, [_ | Css], Cents, 0, [0 | Res], VSs):-
%    eval_segs(Imgseq, Frm, Css, Cents, 0, Res, VSs), !.
%eval_segs(Imgseq, Frm, [S-C | Css], Cents, N, [Re | Res], [VSegs | VSs]):-
%    N > 0, C =\= 0,
%    cross_num(TL),
%    sample_grid_lines_cross_seg_2d(S, TL, Ls),
%    cross_seg_classes(Imgseq, S, Cents, Ls, Crossed_SCs),
%    N1 is N - 1,
%    eval_segs(Imgseq, Frm, Crossed_SCs, Cents, N1, Re1, VSegs1),
%    length(Re1, Total), sum_list(Re1, Valid),
%    Prob is Valid/Total,
%    cross_eval_thresh(TE),
%    % (Prob >= TE ->
%    %      (Re = 1,          
%    %       append(VSegs1, VSegs),
%    %       eval_segs(Imgseq, Frm, Css, Cents, N, Res, VSs), !);
%    %  (Re = 0,
%    %   eval_segs(Imgseq, Frm, Css, Cents, N, Res, [VSegs | VSs]), !)
%    % ).
%    Prob >= TE,
%    Re = 1,
%    mask_select(Re1, VSegs1, VSegs2),
%    pairs_keys(Crossed_SCs, Crossed_Segs),
%    mask_select(Re1, Crossed_Segs, VSegs3),
%    %append(VSegs1, VSegs2),
%    append(VSegs2, VSegs4),
%    append(VSegs3, VSegs4, VSegs),
%    eval_segs(Imgseq, Frm, Css, Cents, N, Res, VSs), !.
%eval_segs(Imgseq, Frm, [_-C | Css], Cents, N, [0 | Res], [[] | VSs]):-
%    C =\= 0,
%    eval_segs(Imgseq, Frm, Css, Cents, N, Res, VSs), !.
%
