% number of random lines for determine background and non-background colors
rand_line_num(50).
% threshold of scharr gradient
thresh_scharr(2).

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
color_bk_nonbk(Imgseq, Frame, BK, NonBK, Longest):-
    write('sampling random lines.'), nl,
    rand_line_num(N),
    size_3d(Imgseq, W, H, _),
    rand_sample_lines(Frame, [W, H], N, Lines),
    thresh_scharr(T),
    sample_lines_hists(Imgseq, Lines, T, SHs),
    cluster_seg_hist_pairs(SHs, 2, SHs_Sign, Cents),
    write('clustering finished.'), nl,
    transpose_pairs(SHs_Sign, Sign_SHs),
    % group the clusters, the larger one is BK
    group_pairs_by_key(Sign_SHs, [0-G0, 1-G1]),
    length(G0, L0), length(G1, L1),
    (L0 >= L1 ->
         (Cents = [BK, NonBK], NBKs = G1, !);
     (Cents = [NonBK, BK], NBKs = G0, !)
    ),
    pairs_keys(NBKs, Segs),
    longest_seg(Segs, Longest).

% randomly sample multiple lines
rand_sample_lines(_, _, 1, []):-
    !.
rand_sample_lines(Frame, [W, H], N, [[C, Dir] | CDs]):-
    W1 is W - 1, H1 is H - 1,
    random_between(0, W1, X), random_between(0, H1, Y),
    random_between(-10, 10, XX), random_between(0, 50, YY), % YY>=0
    C = [X, Y, Frame],
    ((XX == 0, YY == 0) ->
         (Dir = [1, 1, Frame], !);
     (Dir = [XX, YY, Frame], !)),
    N1 is N - 1,
    rand_sample_lines(Frame, [W, H], N1, CDs).

