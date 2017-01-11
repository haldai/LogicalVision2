:- ensure_loaded(['../sampling/plcolor.pl',
                  '../sampling/plsampling.pl']).

% threshold of extrema value, it is the product of derivative
%     before and after the extrema position.
%     e.g. if derivative is ..., -1, 0, 1, ..., we want to detect
%          the position of "0" then thresh is -1*1 = -1.
extrema_thresh(-50).

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
    line_seg_points(Start, End, [W, H, D], Points),
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
    line_seg_points(Start, End, [W, H, D], Points),
    pts_color(Imgseq, Points, Colors),
    column(1, Colors, Lchannel).

%====================
% sample gradients
%====================
% sample_line_L_grad(+Imgseq, +Pt, +Dir, -Points, -Grads)
% sample a line and get its brightness gradients
sample_line_L_grad(Imgseq, Pt, Dir, Points, Grads):-
    sample_line_color(Imgseq, Pt, Dir, Points, Colors),
    grad_l(Colors, Grads), !.

sample_lines_L_grads(_, [], [], []):-
    !.
sample_lines_L_grads(Imgseq, [L | Lines], [Pts | Points], [Grds | Grads]):-
    L = [Pt, Dir],
    sample_line_color(Imgseq, Pt, Dir, Pts, Colors),
    grad_l(Colors, Grds),
    sample_lines_L_grads(Imgseq, Lines, Points, Grads), !.

% sample_line_seg_L_grad(+Imgseq, +Start, +End, -Points, -Grads)
% sample a line segment and get its brightness gradients
sample_line_seg_L_grad(Imgseq, Start, End, Points, Grads):-
    sample_line_seg_color(Imgseq, Start, End, Points, Colors),
    grad_l(Colors, Grads), !.

%===========================
% sample brightness extrema
%===========================
sample_line_L_extrema(Imgseq, Point, Dir, ExtPts):-
    sample_line_L_grad(Imgseq, Point, Dir, Pts, G),
    pairs_keys_values(Pair, G, Pts),
    zero_item(Pair, ExtPts).
sample_line_seg_L_extrema(Imgseq, Start, End, ExtPts):-
    sample_line_seg_L_grad(Imgseq, Start, End, Pts, G),
    pairs_keys_values(Pair, G, Pts),
    zero_item(Pair, ExtPts).

%==========================
% sample gradient extrema
%==========================
sample_line_L_grad_extrema(Imgseq, Point, Dir, ExtPts):-
    sample_line_L_grad(Imgseq, Point, Dir, Pts, G),
    grad(G, G2),
    pairs_keys_values(Pair, G2, Pts),
    zero_item(Pair, ExtPts).
sample_line_seg_L_grad_extrema(Imgseq, Start, End, ExtPts):-
    sample_line_seg_L_grad(Imgseq, Start, End, Pts, G),
    grad(G, G2),
    pairs_keys_values(Pair, G2, Pts),
    zero_item(Pair, ExtPts).

%======================
% sample lines extrema
%======================
sample_lines_L_extrema(_, [], []).
sample_lines_L_extrema(Imgseq, [L | Lines], [P | Pts]):-
    L = [Point, Dir],
    sample_line_L_extrema(Imgseq, Point, Dir, P),
    sample_lines_L_extrema(Imgseq, Lines, Pts), !.

sample_lines_L_grad_extrema(_, [], []).
sample_lines_L_grad_extrema(Imgseq, [L | Lines], [P | Pts]):-
    L = [Point, Dir],
    sample_line_L_grad_extrema(Imgseq, Point, Dir, P),
    sample_lines_L_grad_extrema(Imgseq, Lines, Pts), !.

%=====================
% 2d line sampling
%=====================
% sample_line_color_L_2d(+Img, +Start, +Direct, -Points, -Lchannel)
% sample a line to get its points and cooresponding brightness
sample_line_color_L_2d(Img, Start, Direct, Points, Lchannel):-
    size_2d(Img, W, H),
    line_points_2d(Start, Direct, [W, H], Points),
    pts_color_2d(Img, Points, Colors),
    column(1, Colors, Lchannel). % nth1 starts with index 1

% sample_line_seg_color_L_2d(+Img, +Start, +End, -Points, -Lchannel)
% sample a line segment to get its points and cooresponding brightness
sample_line_seg_color_L_2d(Img, Start, End, Points, Lchannel):-
    size_2d(Img, W, H),
    line_seg_points(Start, End, [W, H], Points),
    pts_color_2d(Img, Points, Colors),
    column(1, Colors, Lchannel).

sample_line_color_2d(Img, Start, Direct, Points, Colors):-
    size_2d(Img, W, H),
    Start = [SX, SY | _], Direct = [DX, DY | _],
    line_points_2d([SX, SY], [DX, DY], [W, H], Points),
    pts_color_2d(Img, Points, Colors).

sample_line_seg_color_2d(Img, Start, End, Points, Colors):-
    size_2d(Img, W, H),
    Start = [SX, SY | _], End = [DX, DY | _],
    line_seg_points_2d([SX, SY], [DX, DY], [W, H], Points),
    pts_color_2d(Img, Points, Colors).

%================================================
% sample a line and get points with color changing
%================================================
% sample a line with direction, get the color of points on line and
% find the points that separates two colors
% colors are defined in plcolor.pl
sample_line_color_change_2d(Img, [Pt, Dir], Color1, Color2, Chgd):-
    sample_line_color_2d(Img, Pt, Dir, Pts, LABs), % get LAB value of each point
    color_change_points(Color1, Color2, Pts, LABs, Chgd).
sample_line_seg_color_change_2d(Img, [Start, End], Color1, Color2, Chgd):-
    sample_line_seg_color_2d(Img, Pts, Start, End, LABs),
    color_change_points(Color1, Color2, Pts, LABs, Chgd).

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

%===============
% random lines
%===============
rand_line_2d([W, H], [X, Y], [DX, DY]):-
    rand_2d_point([X, Y], [W, H]), rand_2d_angle_vec([DX, DY]).
% many lines
rand_lines_2d(_, 0, []):-
    !.
rand_lines_2d([W, H], N, [[[X, Y], [DX, DY]] | Lines]):-
    rand_2d_point([X, Y], [W, H]), rand_2d_angle_vec([DX, DY]),
    N1 is N - 1,
    rand_lines_2d([W, H], N1, Lines).

%====================================
% sample line and get color hists
%====================================
sample_line_color_hist_2d(Img, Pt, Dir, Pts, Hists):-
    size_2d(Img, W, H),
    line_points_2d(Pt, Dir, [W, H], Pts),
    pts_color_hists_2d(Img, Pts, Hists).

sample_line_color_L_hist_2d(Img, Pt, Dir, Pts, Hists):-
    size_2d(Img, W, H),
    line_points_2d(Pt, Dir, [W, H], Pts),
    pts_color_L_hists_2d(Img, Pts, Hists).

sample_lines_color_L_hist_2d(_, [], [], []):-
    !.
sample_lines_color_L_hist_2d(Img, [[Pt, Dir] | Lines], Pts, Hists):-
    sample_line_color_L_hist_2d(Img, Pt, Dir, P1, H1),
    sample_lines_color_L_hist_2d(Img, Lines, P2, H2),
    append(P1, P2, Pts),
    append(H1, H2, Hists).

%===============================================================
% sample a line and get color histogram changing (KL-divergence)
%===============================================================
sample_line_color_L_hist_change_2d(Img, Pt, Dir, Pts, Hist_Chg):-
    sample_line_color_hist_2d(Img, Pt, Dir, Pts, Hists),
    grad_KL(Hists, Hist_Chg).

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

