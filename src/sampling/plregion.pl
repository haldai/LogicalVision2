/* Sampling module (region based)
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

%=============================
% sample variance of a cube
%=============================
sample_cube_var(Imgseq, Center, Radius, Var):-
    size_3d(Imgseq, W, H, D),
    in_cube_points(Center, Radius, [W, H, D], Pts),
    pts_set_var(Imgseq, Pts, Var).

%===========================
% color hist of a cube
%===========================
sample_cube_hist(Imgseq, Center, Radius, Hist):-
    size_3d(Imgseq, W, H, D),
    in_cube_points(Center, Radius, [W, H, D], Pts),
    points_color_hist(Imgseq, Pts, Hist).
