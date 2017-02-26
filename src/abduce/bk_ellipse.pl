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
/* Background knowledge of ellipse
 */
:- ensure_loaded(['../sampling/plsampling.pl']).

%===================================
% make conjectures
%===================================
% discover object in a Frame of ImageSequence
discover_object(_, _, _, [], []):-
    !.
% discover object in a Frame of ImageSequence
discover_object(Imgseq, Frm, Centroids, [S | Segs],
                [elps(Cen, Para, C) | Elpses]):-
    rec_eval_turn(Turn),
    class_of_seg(Imgseq, S, Centroids, C),
    length(Segs, L), write('VSegs #'), write(L), nl,
    eval_segs(Imgseq, Frm, [S-C], Centroids, Turn, _, VSegs1),
    %write(eval_segs(Imgseq, Frm, [S-C], Centroids, Turn, _, VSegs1)), nl,
    VSegs1 \= [[]],
    VSegs1 = [VSegs],
    print_list(VSegs),
    append(VSegs, Edg_Pts_1), list_to_set(Edg_Pts_1, Edg_Pts),
    length(Edg_Pts, LE), LE >= 5,
    fit_elps(Edg_Pts, Cen, Para),
    write('fitted parameters: '), write(Cen), write(', '), write(Para), nl,    
    size_3d(Imgseq, W, H, D),
    remove_duplicate_segs(Segs, [Cen, Para], [W, H, D], Segs1),
    %Segs1 = Segs,

    % DEBUG START
    % write('Current Seg: '), write(S-C), nl,    
    % write('Crossed Segs: '), nl, print_list_ln(Crossed_SCs),
%    size_3d(Imgseq, W, H, D),
%    seq_img(Imgseq, Frm, IMG1),
%    clone_img(IMG1, IMG2),
%    draw_line_segs_2d(IMG2, VSegs, red),
%    ellipse_points(Cen, Para, [W, H, D], Elps),
%    draw_points_2d(IMG2, Elps, green),
%    showimg_win(IMG2, 'debug'),
%    release_img(IMG2),
    % DEBUG END

    discover_object(Imgseq, Frm, Centroids, Segs1, Elpses), !.
discover_object(Imgseq, Frm, Centroids, [_ | Segs], Elps):-
    discover_object(Imgseq, Frm, Centroids, Segs, Elps), !.

% average ellipse (according to parameters) of a list of ellipses
average_elps(Elpses, elps(Cen, Param, Color)):-
    findall(Center, member(elps(Center, _, _), Elpses), Centers),
    findall(A, member(elps(_, [A, _, _], _), Elpses), As),
    findall(B, member(elps(_, [_, B, _], _), Elpses), Bs),
    findall(C, member(elps(_, [_, _, C], _), Elpses), Cs),
    findall(Col, member(elps(_, _, Col), Elpses), Colors),
    average(Centers, Cen),    
    average(As, Av), average(Bs, Bv), average(Cs, Cv),
    Param = [Av, Bv, Cv],
    mode(Colors, Color), !.
    

%%%%%%%%% segment-wise evaluation %%%%%%%%%
eval_segs(_, _, [], _, _, [], []):-
    !.
% class BG
eval_segs(Imgseq, Frm, [_-0 | Css], Cents, N, [0 | Res], VSs):-
    eval_segs(Imgseq, Frm, Css, Cents, N, Res, VSs),
    !.
% level-0: success
eval_segs(Imgseq, Frm, [S-C | Css], Cents, 0, [1 | Res], VSs):-
    C =\= 0,
    cross_num(TL), sample_grid_lines_cross_seg_2d(S, TL, Ls),
    cross_seg_classes(Imgseq, S, Cents, Ls, Crossed_SCs),
    findall(Seg, member(Seg-C, Crossed_SCs), NBGs),
    length(NBGs, Lnbg), length(Crossed_SCs, Total),
    Prob is Lnbg/Total,
    cross_eval_thresh(TE),
    Prob >= TE,
    eval_segs(Imgseq, Frm, Css, Cents, 0, Res, VSs), !.
% level-0: failed
eval_segs(Imgseq, Frm, [_ | Css], Cents, 0, [0 | Res], VSs):-
    eval_segs(Imgseq, Frm, Css, Cents, 0, Res, VSs), !.
eval_segs(Imgseq, Frm, [S-C | Css], Cents, N, [Re | Res], [VSegs | VSs]):-
    N > 0, C =\= 0,
    cross_num(TL),
    sample_grid_lines_cross_seg_2d(S, TL, Ls),
    cross_seg_classes(Imgseq, S, Cents, Ls, Crossed_SCs),
    N1 is N - 1,
    eval_segs(Imgseq, Frm, Crossed_SCs, Cents, N1, Re1, VSegs1),
    length(Re1, Total), sum_list(Re1, Valid),
    Prob is Valid/Total,
    cross_eval_thresh(TE),
    % (Prob >= TE ->
    %      (Re = 1,          
    %       append(VSegs1, VSegs),
    %       eval_segs(Imgseq, Frm, Css, Cents, N, Res, VSs), !);
    %  (Re = 0,
    %   eval_segs(Imgseq, Frm, Css, Cents, N, Res, [VSegs | VSs]), !)
    % ).
    Prob >= TE,
    Re = 1,
    mask_select(Re1, VSegs1, VSegs2),
    pairs_keys(Crossed_SCs, Crossed_Segs),
    mask_select(Re1, Crossed_Segs, VSegs3),
    %append(VSegs1, VSegs2),
    append(VSegs2, VSegs4),
    append(VSegs3, VSegs4, VSegs),
    eval_segs(Imgseq, Frm, Css, Cents, N, Res, VSs), !.
eval_segs(Imgseq, Frm, [_-C | Css], Cents, N, [0 | Res], [[] | VSs]):-
    C =\= 0,
    eval_segs(Imgseq, Frm, Css, Cents, N, Res, VSs), !.
