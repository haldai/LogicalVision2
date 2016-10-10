% number of random lines for determine background and non-background colors
rand_line_num(200).
% threshold of scharr gradient
thresh_scharr(5).

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
color_bk_nonbk(Imgseq, Frame, [BK_Centroid, NonBK_Centroids], NonBK_Segs):-
    write('sampling random lines.'), nl,
    rand_line_num(N),
    size_3d(Imgseq, W, H, _),
    rand_sample_lines(Frame, [W, H], N, Lines),
    thresh_scharr(T),
    sample_lines_hists(Imgseq, Lines, T, SHs),
    cluster_seg_hist_pairs(SHs, 4, SHs_Sign, Cents),
    write('clustering finished.'), nl,
    transpose_pairs(SHs_Sign, Sign_SHs),
    % group the clusters, the larger one is BK
    group_pairs_by_key(Sign_SHs, [0-G0, 1-G1, 2-G2, 3-G3]),
    % the group with maximum local length is background
    total_seg_length(G0, L0),
    total_seg_length(G1, L1),
    total_seg_length(G2, L2),
    total_seg_length(G3, L3),
    write(L0), write(', '), write(L1), write(', '),
    write(L2), write(', '), write(L3), nl,
    max_list([L0, L1, L2, L3], Max),
    switch(Max, [L0:
                 (Cents = [BK_Centroid, NonBK1, NonBK2, NonBK3],
                  append([G1, G2, G3], NBKs)),
                 L1:
                 (Cents = [NonBK1, BK_Centroid, NonBK2, NonBK3],
                  append([G0, G2, G3], NBKs)),
                 L2:
                 (Cents = [NonBK1, NonBK2, BK_Centroid, NonBK3],
                  append([G0, G1, G3], NBKs)),
                 L3:
                 (Cents = [NonBK1, NonBK2, NonBK3, BK_Centroid],
                  append([G0, G1, G2], NBKs))
                ]
          ),
    NonBK_Centroids = [NonBK1, NonBK2, NonBK3],
%    (L0 >= L1 ->
%         (Cents = [BK, NonBK], NBKs = G1, !);
%     (Cents = [NonBK, BK], NBKs = G0, !)
%    ),
    pairs_keys(NBKs, NonBK_Segs).
%    longest_seg(NonBK_Segs, Longest).

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
% determine whether a line segment belongs to background
%========================================================
%seg_is_bk(Imgseq, [P1, P2], [BK_Centroid, NonBK_Centroids]):-
%    size_3d(Imgseq, W, H, D),
%    line_seg_points(P1, P2, [W, H, D], Pts),
%    points_color_hist(Imgseq, Pts, Hist),
%    eu_dist(Hist, BK, D1),
%    eu_dist(Hist, NonBK, D2),
%    D1 >= D2.

class_of_seg(Imgseq, Seg, [BK_Centroid, NonBK_Centroids], Class):-
    append([BK_Centroid], NonBK_Centroids, Cents),
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
% Cents = [BK, [NonBK1, NonBK2, NonBK3]].
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
sample_seg_get_classes(Imgseq, Centroids, Point, Dir, Seg_Classes):-
    thresh_scharr(Thresh_Scharr),
    line_pts_scharr_geq_T(Imgseq, Point, Dir, Thresh_Scharr, Edge_pts),
    class_of_segs(Imgseq, Edge_pts, Centroids, Seg_Classes).

%===================================
% make conjectures
%===================================
% discover object in a Frame of ImageSequence
discover_object(Imgseq, Frm, [BK_Centroid, NonBK_Centroids], Buff, Return):-
    fail.
