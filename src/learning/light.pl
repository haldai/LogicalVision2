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
:- ['metagol'].

:- ensure_loaded(['../io/plio.pl',
                  '../sampling/plsampling.pl',
                  '../drawing/pldraw.pl',
                  '../stats/plstats.pl',
                  '../abduce/plabduce.pl',
                  '../abduce/bk_object.pl',
                  '../sampling/plpoints.pl',
                  '../utils/utils.pl']).

%% metagol parameters
metagol:max_clauses(8).
max_time(600000). % 10 min timeout

%% primitives
prim(light_source_angle/3).
prim(highlight/2).
prim(opposite_angle/2).

%% background knowledge
highlight(obj1, obj2). % obj2 is obj1's highlight accroding to LogVis

% opposite angles in label
opposite_angle(3, 9).
opposite_angle(9, 3).
opposite_angle(12, 6).
opposite_angle(6, 12).

% "obj1" is the abduced object, "obj2" is the highlight part
logical_vision_light(Name, clock_angle(obj1, obj2, Ang)):-
    % Path = '../../data/protists/',
    Path = '../../data/moons/',
    atomic_concat(Name, '.jpg', File),
    atomic_concat(Path, File, File_Path),
    write('Load image: '), write(File_Path), nl,
    load_img(File_Path, Img),
    % load_model_svm('../../tmp/SVM_Protist.model', Model),
    load_model_svm('../../tmp/SVM_Moon.model', Model),
    ((% time(abduce_object_elps(Img, Model, [], Obj, 0)),
      time(abduce_object_circle(Img, Model, [], Obj, 0)),
      write('Abduced object: '), writeln(Obj),
      get_largest_contrast_angle(Img, Obj, Ang));
     (Ang = 'nil')
    ), !,
    release_img(Img),
    release_model_svm(Model).

% in training data, all light source are "light"
name2label(Name, light_source_angle(obj1, light, Ang)):-
    string_codes(Name, [D1, D2 | _]), string_codes(Code, [D1, D2]),
    atom_number(Code, Ang0), % label
    (((Ang0 == 11; Ang0 == 12; Ang0 == 1), !, Ang = 12);
     ((Ang0 == 2; Ang0 == 3; Ang0 == 4), !, Ang = 3);
     ((Ang0 == 5; Ang0 == 6; Ang0 == 7), !, Ang = 6);
     ((Ang0 == 8; Ang0 == 9; Ang0 == 10), !, Ang = 9)), !.

%% metarules
metarule(prop_obj1, [P,obj1], ([P,obj1]:-[])).
metarule(prop_obj2, [P,obj2], ([P,obj2]:-[])).
metarule(prop_light, [P,light], ([P,light]:-[])).
metarule(parallel3, [P,Q,R], ([P,A,B,C]:-[[Q,A,B,C],[R,A,B,C]])).
metarule(chain3_3_3, [P,Q,R], ([P,A,B,C]:-[[Q,A,B,D],[R,A,D,C]])).
metarule(chain3_3_2, [P,Q,R], ([P,A,B,C]:-[[Q,A,B,D],[R,D,C]])).
metarule(independent3_2_1_1, [P,Q,R,S], ([P,A,B,C]:-[[Q,A,B],[R,A],[S,C]])).

%% learning
a(Name):-
    % use logical vision to abduce input example for MIL
    name2label(Name, Fact),
    logical_vision_light(Name, Example),
    %% examples without LogVis for debugging
    % Fact = light_source_angle(obj1, light, 12),
    % Example = clock_angle(obj1, obj2, 12),
    %Example = clock_angle(obj1, obj2, 6), % highlight opposite to light source
    assertz(Fact),
    learn([Example], [], P),
    retract(Fact),
    pprint(P).
