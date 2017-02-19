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
:- ensure_loaded(['../learning/metagol.pl',
                  '../io/plio.pl',
                  '../sampling/plsampling.pl',
                  '../drawing/pldraw.pl',
                  '../stats/plstats.pl',
                  '../abduce/bk_object.pl']).

:- use_module('../learning/metagol').

%% primitives
% prim(object/2). % object(Img, circle(Center, Rad))
prim(edge_point_proportion/3). % edge_point_proportion(Img, circle(Center, Rad), Prop)
prim(larger_than_prob/2).
% prim(reasonable_fit/2). % target: reasonable_fit(Img, circle(Center, Rad))

%% background knowledge
sample_ep_num(36).
ep_range(3).

thresh(1.0).  thresh(0.95). thresh(0.9).  thresh(0.85). thresh(0.8).
thresh(0.75). thresh(0.7).  thresh(0.65). thresh(0.6).  thresh(0.55).
thresh(0.5).  thresh(0.45). thresh(0.4).  thresh(0.35). thresh(0.3).
thresh(0.25). thresh(0.2).  thresh(0.15). thresh(0.1).  thresh(0.05).

larger_than_prob(X, P):-
    thresh(P), X >= P, !.

edge_point_proportion(Img_Name, circle(Cent, Rad), Prop):-
    % init
    Path = '../../data/moons/',
    atomic_concat(Img_Name, '.jpg', Img_File),
    atomic_concat(Path, Img_File, File_Path),
    load_img(File_Path, Img),
    load_model_svm('../../tmp/SVM_Protist.model', Model),
    % do jobs
    sample_ep_num(Num),
    eval_edge_points(Img, Model, circle(Cent, Rad), Num, EPs),
    length(EPs, Len),
    Prop is Len/Num,
    release_img(Img),
    release_model_svm(Model).

eval_edge_points(_, _, circle(_, _), 0, []):-
    !.
eval_edge_points(Img, Model, circle(Cent, Rad), Num, [EP | EPs]):-
    V0 = [100, 0], sample_ep_num(Total),
    Sep is 360.0/Total, Ang is Num*Sep,
    vec_rotate_angle_clockwise(V0, V1, Ang),
    vec_sum(V0, Cent, P0), vec_sum(V1, Cent, P1),
    arc_point_between(circle(Cent, Rad), P0, P1, 1.0, P),
    % has edge point on fitted edge
    in_canvas(P, Img),
    ep_range(Range),
    edge_point_in_dir_range(Img, Model, Cent, P, Range, EP),
    EP \= [],
    % continue
    Num1 is Num - 1,
    eval_edge_points(Img, Model, circle(Cent, Rad), Num1, EPs), !.
eval_edge_points(Img, Model, circle(Cent, Rad), Num, EPs):-
    Num1 is Num - 1,
    eval_edge_points(Img, Model, circle(Cent, Rad), Num1, EPs), !.

in_canvas(P, Img):-
    size_2d(Img, W_, H_),
    W is W_ - 1, H is H_ - 1,
    in_box([[0, 0], [W, H]], P).

%% metarules
metarule([P,Q,R,T], ([P,A,B]:-[[Q,A,B,C],[R,C,T]])).

%% learning
a :-
    Pos = [
        reasonable_fit('01001', circle([140,175],112)),
        reasonable_fit('01002', circle([156,172],121))        
    ],
    Neg = [
        reasonable_fit('03026', circle([268,160],172)),
        reasonable_fit('01002', circle([258,8],243))
    ],
    learn(Pos, Neg, Prog),
    pprint(Prog).
