% number of random lines for determine background and non-background colors
rand_line_num(500).
num_clusters(2).
% threshold of scharr gradient
thresh_scharr(5).
% threshold of cross segment sampling
cross_thresh(5).
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
    rand_sample_lines(Frame, [W, H], N, Lines),
    thresh_scharr(T),
    sample_lines_hists(Imgseq, Lines, T, SHs),
    num_clusters(Num_Clusters),
    get_centroids(SHs, Num_Clusters, Centroids, NonBG_Segs).
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
    % NonBG_Centroids = [NonBG1, NonBG2, NonBG3],
%    (L0 >= L1 ->
%         (Cents = [BG, NonBG], NBGs = G1, !);
%     (Cents = [NonBG, BG], NBGs = G0, !)
%    ),
    pairs_keys(NBGs, NonBG_Segs).
%    longest_seg(NonBG_Segs, Longest).

% randomly sample multiple lines
rand_sample_lines(_, _, 1, []):-
    !.
rand_sample_lines(Frame, [W, H], N, [[C, Dir] | CDs]):-
    W1 is W - 1, H1 is H - 1,
    random_between(0, W1, X), random_between(0, H1, Y),
    random_between(-10, 10, XX), random_between(-64, 64, YY), % YY>=0
    C = [X, Y, Frame],
    ((XX == 0, YY == 0) ->
         (Dir = [1, 1, Frame], !);
     (Dir = [XX, YY, Frame], !)),
    N1 is N - 1,
    rand_sample_lines(Frame, [W, H], N1, CDs).

% calculate the total length of a list of line segments
total_seg_length([], 0):-
    !.
total_seg_length([[Start, End]-_ | Segs], Sum):-
    eu_dist(Start, End, Dist),
    total_seg_length(Segs, Sum1),
    !,
    Sum is Sum1 + Dist.
total_seg_length([[Start, End] | Segs], Sum):-
    eu_dist(Start, End, Dist),
    total_seg_length(Segs, Sum1),
    !,
    Sum is Sum1 + Dist.
    

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

%==================================
% edge points on line to segments
%==================================
pts2segs([_], []):-
    !.
pts2segs([P1, P2 | Ps], [[P1, P2] | Ss]):-
    pts2segs([P2 | Ps], Ss).

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

%===================================
% make conjectures
%===================================
% discover object in a Frame of ImageSequence
discover_object(Imgseq, Frm, Centroids, [S | Segs], Buf, Return):-
    % get seg class
    class_of_seg(Imgseq, S, Centroids, C),
    % cross segment sampling
    cross_thresh(TL), sample_lines_cross_seg_2d(S, TL, Ls),
    % eval each line and get classes, calculate the propotion of class "C"
    cross_seg_classes(Imgseq, S-C, Centroids, Ls, Cs),
    sum_list(Cs, Ones), length(Cs, Total),
    Prop is Ones/Total,
    % TODO: if success, save Cs, put into buff
    fail.
