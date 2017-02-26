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
%%%%%%%%%%%%%%%%%%%%%%%%%
% learn object pairing
%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module('metagol').

% target hypothesis
% A = elps(Cen1, Para1, Color), B = elps(Cen2, Para2, Color)
%same_obj(A, B):-
%    integer_(N),
%    same_obj_1(A, B, N).
%same_obj_1(A, B, 0):-
%    thresh_1(T),
%    similar(A, B, T).
%same_obj_1(A, B, N):-
%    sample_mid(A, B, C),
%    minus1(N, N1),
%    same_obj_1(A, C, N1),
%    same_obj_1(B, C, N1).

%% another hypothesis
% same_obj(A, B):-
%     same_obj_1(A, C),
%     same_obj_1(C, B).
% same_obj_1(A, B):-
%     sample_mid(A, B, C),
%     similar(A, C),
%     similar(B, C).

%==================
% primitives of BK
%==================
prim(sample_mid/3).
%prim(discover_obj/2).
prim(similar/3).
%prim(minus1/2).
%prim(integer_/1).

%=============
% metarules
%=============
%metarule(base_recur,[P,Q,N],([P,A,B]:-[[integer_,N],[Q,A,B,N]])):-
%    writeln('base_recur is called !').
%metarule(base_recur_0,[P,Q],
%         ([P,A,B,0]:-[[thresh_1,T],[Q,A,B,T]])):-
%    writeln('base_recur_0 is called !').
%metarule(num_recur,[P,Q,S,T],
%         ([P,A,B,N]:-[[Q,A,B,C],[minus1,N,N1],[S,A,C,N1],[T,B,C,N1]]), PS):-
%    writeln('num_recur is called !'),
%    member(sym(S,3,_),PS),
%    member(sym(T,3,_),PS).

metarule(chain,[P,Q],([P,A,B]:-[[Q,A,C],[Q,C,B]])).
metarule(abducive_chain,[P,Q,R,T],([P,A,B]:-[[Q,A,B,C],[R,A,C,T],[R,C,B,T]])).

%=============================
% generate training examples
%=============================
validate_pair_examples(_, [], [], []):-
    !.
validate_pair_examples(Imgseq, [[Elps1, Elps2] | Elpses], [P | Pos], Neg):-
    Elps1 = elps([_, _, Frm1], _, _),
    Elps2 = elps([_, _, Frm2], _, _),
    seq_img(Imgseq, Frm1, Img1_), seq_img(Imgseq, Frm2, Img2_),
    clone_img(Img1_, Img1), clone_img(Img2_, Img2),
    draw_elpses(Img1, [Elps1]), draw_elpses(Img2, [Elps2]),
    show2imgs_win(Img1, Img2, 'Same Object or Not? Please input [Y]es/[N]ot.'),
    read(Correct),
    (Correct == 'Y'; Correct == 'y'; Correct = true), !,
    P = same_obj(Elps1, Elps2),
    validate_pair_examples(Imgseq, Elpses, Pos, Neg), !.
validate_pair_examples(Imgseq, [[Elps1, Elps2] | Elpses], Pos, [N | Neg]):-
    N = same_obj(Elps1, Elps2),
    validate_pair_examples(Imgseq, Elpses, Pos, Neg).

%=========================
% examples for testing
%=========================
pos([same_obj(elps([345,146,0],[16,37,88],1),elps([280,120,700],[18,29,98],1)), same_obj(elps([484,254,0],[8,10,54],1),elps([480,257,700],[6,14,15],1))]).

neg([same_obj(elps([520,319,0],[9,25,86],1),elps([69,12,700],[18,35,170],1)), same_obj(elps([520,319,0],[9,25,86],1),elps([298,178,700],[16,21,162],1)), same_obj(elps([328,158,0],[26,46,66],1),elps([298,178,700],[16,21,162],1))]).

%======================
% Background knowledge
%======================
color_centroids([[4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.869696916016704e-6,4.8440696335557165e-5,4.869696916016704e-6,4.869696916016704e-6,0.00015409461206019592,4.869696916016704e-6,4.869696916016704e-6,0.00018913725951827623,4.869696916016704e-6,4.869696916016704e-6,0.0018700082171328992,4.869696916016704e-6,0.00029136619737120395,0.0069453171136839185,9.553469470989678e-5,0.0435217641737176,0.04761912261366655,0.0036315788389233885,0.955596317687981,0.6805566780158628,0.023556719900792014,0.00044111967757305383,0.13758560135932754,0.9713860973493321,1.7950446623384624e-5,0.06738269017740225,0.0011985873995081693,4.869696916016704e-6,0.02655487663672469,4.869696916016704e-6,4.869696916016704e-6,0.011746979232760319,4.869696916016704e-6,4.869696916016704e-6,0.0055774038459499515,4.869696916016704e-6,4.869696916016704e-6,0.00507713259752447,4.869696916016704e-6,4.869696916016704e-6,0.0023969806938197594,4.869696916016704e-6,4.869696916016704e-6,0.001381641268333998,4.869696916016704e-6,4.869696916016704e-6,0.0014190915829999484,4.869696916016704e-6,4.869696916016704e-6,0.0012080591002767826,4.869696916016704e-6,4.869696916016704e-6,0.0007271760923470255,4.869696916016704e-6,4.869696916016704e-6,0.0007641510149994263,4.869696916016704e-6,4.869696916016704e-6,0.00026195156709415,4.869696916016704e-6,4.869696916016704e-6,0.00014004671655920868,4.869696916016704e-6,4.869696916016704e-6,0.00014912998876212395,4.869696916016704e-6,4.869696916016704e-6,0.00020046232461430312,4.869696916016704e-6,4.869696916016704e-6], [[1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,1.0572513673514758e-5,0.008548970472116595,1.0572513673514758e-5,1.0572513673514758e-5,0.02200786774495554,1.0572513673514758e-5,1.0572513673514758e-5,0.01528812674630223,1.0572513673514758e-5,1.0572513673514758e-5,0.038513399020142475,1.0572513673514758e-5,0.03640106860103242,0.1318564008682502,0.0012546030714869804,0.6846231651919289,0.14938901478485647,0.02243318845957392,0.2523732950638324,0.17096184493734104,0.053241754752010644,0.024613045015819953,0.182293370404362,0.8237475316230305,0.0017039682582006724,0.10510427908303967,0.09863352924736915,1.0572513673514758e-5,0.05606353833169331,0.00041450749101671366,1.0572513673514758e-5,0.03174356393537653,1.0572513673514758e-5,1.0572513673514758e-5,0.01481295930923981,1.0572513673514758e-5,1.0572513673514758e-5,0.008595613306405505,1.0572513673514758e-5,1.0572513673514758e-5,0.008401784951579686,1.0572513673514758e-5,1.0572513673514758e-5,0.004036207941207786,1.0572513673514758e-5,1.0572513673514758e-5,0.0037562602655898244,1.0572513673514758e-5,1.0572513673514758e-5,0.004979771285992761,1.0572513673514758e-5,1.0572513673514758e-5,0.004167490298069752,1.0572513673514758e-5,1.0572513673514758e-5,0.005514142371052652,1.0572513673514758e-5,1.0572513673514758e-5,0.005120104741349841,1.0572513673514758e-5,1.0572513673514758e-5,0.0032058614117634393,1.0572513673514758e-5,1.0572513673514758e-5,0.006546423539377301,1.0572513673514758e-5,1.0572513673514758e-5,0.018997851626872873,1.0572513673514758e-5,1.0572513673514758e-5]]]).

sample_mid(elps(A, _, _), elps(B, _, _), C):-
    ground(A), ground(B),
    mid_point(A, B, M),
    discover_obj(M, C).

% discover objects given the position of one of its inner point
discover_obj(Point, elps(Center, Parameter, Color)):-
    imgseq(Imgseq),
    color_centroids(Centroids),
    size_3d(Imgseq, W, H, D),
    Point = [_, _, Frm],
    random_line_segs_crossing_point(Imgseq, Point, [W, H, D], Segs),
    discover_object(Imgseq, Frm, Centroids, Segs, Ellipses),
    average_elps(Ellipses, elps(Center, Parameter, Color)).

similar(elps(_, Para1, C1), elps(_, Para2, C2), T):-
    ground(Para1), ground(Para2),
    Para1 = [A1, B1, _],
    Para2 = [A2, B2, _],
    D is abs(A1*B1 - A2*B2)/10, % area size is close
    thresh_1(T),
    D =< T,
    C1 == C2. % same color class

% minus1(0, _):-
%     fail.
% minus1(N, N1):-
%     integer(N),
%     N1 is N - 1.
% 
thresh_1(1).
thresh_1(2).
thresh_1(4).
thresh_1(8).
thresh_1(16).
thresh_1(32).
thresh_1(64).
% 
% integer_(3).
% integer_(2).
% integer_(1).


%===========
% learning
%===========
a:-
    load_video('../../data/Protist.mp4', Vid),
    video2imgseq(Vid, Imgseq),
    release_video(Vid), % release video to save memory
    writeln('%% Video loaded.'),
    assertz(imgseq(Imgseq)),

    pos(Pos),
    neg(Neg),
    learn(Pos,Neg,Prog),
    pprint(Prog),
    release_imgseq(Imgseq), !.

% result1:
% same_obj(A,B):-sample_mid(A,B,C),similar(A,C,8),similar(C,B,8).
