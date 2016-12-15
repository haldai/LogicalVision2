:- ensure_loaded(['../sampling/plcolor.pl']).

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

sample_line_color_2d(Img, Start, Direct, Points, Colors):-
    size_2d(Img, W, H),
    Start = [SX, SY | _], Direct = [DX, DY | _],
    line_points([SX, SY, 0], [DX, DY, 0], [W, H, 1], Points),
    pts_color_2d(Img, Points, Colors).

% sample_line_seg_color(+Imgseq, +Start, +End, -Points, -Colors)
% sample a line segment to get its points and cooresponding color
sample_line_seg_color(Imgseq, Start, End, Points, Colors):-
    size_3d(Imgseq, W, H, D),
    line_seg_points(Start, End, [W, H, D], Points),
    pts_color(Imgseq, Points, Colors).

sample_line_seg_color_2d(Img, Start, End, Points, Colors):-
    size_2d(Img, W, H),
    Start = [SX, SY | _], End = [DX, DY | _],
    line_seg_points([SX, SY, 0], [DX, DY, 0], [W, H, 1], Points),
    pts_color_2d(Img, Points, Colors).

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
grad([], [], _):-
    !.
grad([L | Ls], [G | Gs], start):-
    G = 0,
    grad(Ls, Gs, L), !.
grad([L | Ls], [G | Gs], P):-
    G is L - P,
    grad(Ls, Gs, L), !.

% grad_column(+Column, +List_of_LAB, -Grad)
grad_column(Col, List_of_Vec, Grad):-
    column(Col, List_of_Vec, Column),
    grad(Column, Grad).

%=============================================
% zero item: return the items that key is 0.0
%=============================================
zero_item(L, Re):-
    extrema_thresh(T), zero_item(L, T, Re).

zero_item([_, _], _, []):-!.
zero_item([K1-I, K2-I2 | Pairs], T, [I | Is]):-
    K1*K2 < T,
    zero_item([K2-I2 | Pairs], T, Is).
zero_item([K1-_, Z1-I, K2-I2 | Pairs], T, [I | Is]):-
    K1*K2 < T,
    Z1 =:= 0,
    zero_item([K2-I2 | Pairs], T, Is), !.
zero_item([K1-_, Z1-I, Z2-_, K2-I2 | Pairs], T, [I | Is]):-
    K1*K2 < T,
    Z1 =:= 0, Z2 =:= 0,
    zero_item([K2-I2 | Pairs], T, Is), !.
zero_item([K1-_, Z1-_, Z2-I, Z3-_, K2-I2 | Pairs], T, [I | Is]):-
    K1*K2 < T,
    Z1 =:= 0, Z2 =:= 0, Z3 =:= 0,
    zero_item([K2-I2 | Pairs], T, Is), !.
zero_item([K1-_, Z1-_, Z2-I, Z3-_, Z4-_, K2-I2 | Pairs], T, [I | Is]):-
    K1*K2 < T,
    Z1 =:= 0, Z2 =:= 0, Z3 =:= 0, Z4 =:= 0,
    zero_item([K2-I2 | Pairs], T, Is), !.
zero_item([K1-_, Z1-_, Z2-_, Z3-I, Z4-_, Z5-_, K2-I2 | Pairs], T, [I | Is]):-
    K1*K2 < T,
    Z1 =:= 0, Z2 =:= 0, Z3 =:= 0, Z4 =:= 0, Z5 =:= 0,
    zero_item([K2-I2 | Pairs], T, Is), !.
zero_item([_-_, K-I | Pairs], T, Is):-
    zero_item([K-I | Pairs], T, Is), !.

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
