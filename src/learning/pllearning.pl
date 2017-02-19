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
along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
************************************************************************/
%%%%%%%%%%%%%%%%%%%%%%%%%
% library for learning
%%%%%%%%%%%%%%%%%%%%%%%%%
:- ensure_loaded('metagol.pl'),
   ensure_loaded('pairing.pl').

learn(pairing):-
    %% load data
    load_video('../../data/Protist.mp4', Vid),
    video2imgseq(Vid, Imgseq),
    release_video(Vid), % release video to save memory
    size_3d(Imgseq, W, H, D), write('W x H x D: '),
    write(W), write(' x '), write(H), write(' x '), write(D), nl,
    %% get color classes
    D1 is D - 1, random_between(0, D1, Rand_Frame),
    color_bg_nonbg(Imgseq, Rand_Frame, Cents, _),
    writeln('Centroids: '), print_list(Cents),
    %% ellipses on frame 0
    rand_sample_lines(0, [W, H], 300, Lines1),
    get_non_bg_segs(Imgseq, Cents, Lines1, Segs1),
    discover_object(Imgseq, 0, Cents, Segs1, Elpses1),
    print_list_ln(Elpses1),
    seq_img(Imgseq, 0, Img1_),
    clone_img(Img1_, Img1),
    draw_elpses(Img1, Elpses1),
    %showimg_win(Img1, 'frame 0'),
    %% ellipses on frame 700
    rand_sample_lines(700, [W, H], 300, Lines2),
    get_non_bg_segs(Imgseq, Cents, Lines2, Segs2),
    discover_object(Imgseq, 700, Cents, Segs2, Elpses2),
    print_list_ln(Elpses2),
    seq_img(Imgseq, 700, Img2_),
    clone_img(Img2_, Img2),
    draw_elpses(Img2, Elpses2),
    %showimg_win(Img2, 'frame 700'),
    show2imgs_win(Img1, Img2, '0 vs 700'),
    release_img(Img1),
    release_img(Img2),
    %% Build training examples
    cartesian_product(Elpses1, Elpses2, Elpse_Pairs),
    validate_pair_examples(Imgseq, Elpse_Pairs, Pos, Neg),
    writeln('Positive Examples: '), print_list(Pos),
    writeln('Negative Examples: '), print_list(Neg),
    %% Ending
    release_imgseq(Imgseq).

