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
:- ['metagol_ai'].

:- ensure_loaded(['../io/plio.pl',
                  '../sampling/plsampling.pl',
                  '../drawing/pldraw.pl',
                  '../stats/plstats.pl',
                  '../abduce/plabduce.pl',
                  '../abduce/bk_object.pl',
                  '../sampling/plpoints.pl',
                  '../utils/utils.pl']).

% metagol:functional.
metagol:max_clauses(3).
max_time(600000). % 10 min timeout

%% primitives
prim(observer/1).
prim(reflector/1).
prim(contains/2).
prim(brighter/2).
prim(light_path/2).
prim(opposite_angle/2).
% Abducibles
% prim(convex/1).
% prim(convey/1).
% prim(light_source/1).
% prim(light_source_angle/3).

%% background knowledge
light_source(light).
observer(observer).
reflector(obj2).
contains(obj1,obj2).
brighter(obj2,obj1).

unobstructed(light,obj2).
unobstructed(obj2,observer).

% opposite angles in label
opposite_angle(3, 9).
opposite_angle(9, 3).
opposite_angle(12, 6).
opposite_angle(6, 12).

light_path(X,X).
light_path(X,Y) :- unobstructed(X,Z), light_path(Z,Y).

logical_vision_light(clock_angle(obj1, obj2, Ang)):-
    Path = '../../data/',
    atomic_concat('crater', '.png', File),
    atomic_concat(Path, File, File_Path),
    load_img(File_Path, Img),
    load_model_svm('../../tmp/SVM_crater.model', Model),
    ((time(abduce_object_circle(Img, Model, [], Obj, 0)),
      write('Abduced object: '), writeln(Obj),
      get_largest_contrast_angle(Img, Obj, Ang));
     (Ang = 'nil')
    ), !,
    clone_img(Img, Img2),
    size_2d(Img, W, H), Obj = circle(Cen, Rad),
    circle_points_2d(Cen, Rad, [W, H], Pts),
    draw_points_2d(Img2, Pts, red),
    showimg_win(Img2, 'fitted object'),
    % save_img(Img2, '../../tmp/tmp.png'),
    release_img(Img2),
    release_img(Img),
    release_model_svm(Model).

% interpreted background knowledge
interpreted(clock_angle/3).
interpreted(highlight/2).
background(([clock_angle,O,H,A] :- [[highlight,O,H], [convex,O],
	                                [light_source,L],
                                    [light_source_angle,O,L,A]])).
background(([clock_angle,O,H,A] :- [[highlight,O,H], [convey,O],
	                                [light_source,L],
                                    [light_source_angle,O,L,A1],
                                    [opposite_angle,A1,A]])).
background(([highlight,Obj,H] :- [[contains,Obj,H], [brighter,H,Obj],
	[light_source,L], [light_path,L,H], [reflector,H],
	[light_path,H,Obs], [observer,Obs]])).

% clock_angle(O,H,A) :- highlight(O,H), convex(O), light_source(L),
%	light_source_angle(O,L,A).
% clock_angle(O,H,A) :- highlight(O,H), convex(O), light_source(L),
%	light_source_angle(O,L,A1),opposite_angle(A).
% highlight(X,Y) :- contains(X,Y), brighter(Y,X),
%	light_source(L), light_path(L,R), reflector(R),
% 	light_path(R,O), observer(O).

%% metarules
metarule(1,[P,A],([P,A]:-[]),_).
metarule_init(1,[P,A],([P,A]:-[])).
metarule(2,[P,A,B],([P,A,B]:-[]),_).
metarule_init(2,[P,A,B],([P,A,B]:-[])).
metarule(3,[P,A,B,C],([P,A,B,C]:-[]),_).
metarule_init(3,[P,A,B,C],([P,A,B,C]:-[])).

%% learning
a:-
    logical_vision_light(Obj_clock_angle),
    write("Logical Vision result: "), writeln(Obj_clock_angle),
    findall(G,learn([Obj_clock_angle],[],G),Gs),
    list_to_set(Gs, Gss),
    length(Gss, Len),
    write("\nNum of abduced programs: "), writeln(Len), nl,
    pprint_all(Gss).

pprint_all([]).
pprint_all([G | Gs]):-
    pprint(G), write("\n"),
    pprint_all(Gs).
