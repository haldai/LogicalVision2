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
/* Generate superpixel data for football experiment
 * ===================================================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

:- ensure_loaded(['test3.pl']).

labeling_proportion(0.95).

select_sp_data(ID):-
    Types = [0, 1, 2],
    Sizes = [10, 15, 20, 25, 30],
    cartesian_product(Types, Sizes, Comb),
    select_best_ball_sp(ID, Comb, 10000, -10000).

select_best_ball_sp(_, [], _, _):-
    !.
select_best_ball_sp(ID, [[T, S] | Comb], Tmp_num, Tmp_area):-
    atomic_list_concat(['../../out/SP', T, S, 'Relational'], '/', Path),
    atomic_list_concat(['../../out/SP', T, S, 'Statistical'], '/', SPath),
    atomic_concat(ID, '.csv', SPFile),
    atomic_concat(ID, '.txt', SLFile),
    atomic_list_concat([SPath, SPFile], '/', SPPath),
    atomic_list_concat([SPath, SLFile], '/', SLPath),
    atomic_concat(ID, '_labels.pl', LFile),
    atomic_concat(ID, '_bk.pl', BFile),
    atomic_list_concat([Path, LFile], '/', LPath),
    atomic_list_concat([Path, BFile], '/', BPath),
    OPath = '../../out/SP/final/Relational/',
    OSPath = '../../out/SP/final/Statistical/',
    % load superpixels & bounding box
    load_superpixels(SPPath, SP),
    football(ID, Box),
    % load labels
    consult(LPath),
    ball_sp(ID, SPs),
    length(SPs, Num),
    get_sps_pixels(SP, SPs, Pts),
    points_in_box(Box, Pts, NPts),

    %% debug
    write('\t--- '), write(T), write(', '), writeln(S),
    Dir = '../../data/MobileRobotAndBall1/raw_images/',
    atomic_list_concat([Dir, ID], '/', ImgName),
    atomic_concat(ImgName, '.jpg', ImgPath),    
    load_img(ImgPath, Img),
    show_superpixels(Img, SP),
    clone_img(Img, Img2),
    draw_points_2d(Img2, Pts, red),
    showimg_win(Img2, 'debug'),
    %release_sp(SP),
    release_img(Img),
    release_img(Img2),    
    %% debug end
    
    % unload
    unload_file(LPath),
    release_sp(SP),

    % judging
    Num > 0,
    (Num < Tmp_num;
     (Num == Tmp_num,
      NPts > Tmp_area)
    ), !,

    write('\t'), write(Num), write(', '), write(NPts), nl,
    
    % copy
    atomic_list_concat([OPath, LFile], '/', OLPath),
    atomic_list_concat([OPath, BFile], '/', OBPath),
    atomic_list_concat([OSPath, SPFile], '/', OSPPath),
    atomic_list_concat([OSPath, SLFile], '/', OSLPath),
    copy_file(LPath, OLPath),
    copy_file(BPath, OBPath),
    copy_file(SPPath, OSPPath),
    copy_file(SLPath, OSLPath),

    % continue
    select_best_ball_sp(ID, Comb, Num, NPts), !.
select_best_ball_sp(ID, [_ | Comb], Tmp_num, Tmp_area):-
    select_best_ball_sp(ID, Comb, Tmp_num, Tmp_area).

process_directory:-
    Dir = '../../data/MobileRobotAndBall1/raw_images/',
    ['../../out/SP/objects_and_annotations/football.pl'],
    directory_files(Dir, Files),
    remove_files_extension(Files, Names),
    forall(%member(Name, Names),
           member(Name, ["4760", "4615", "5010", "1410", "5975"]),
           (writeln(Name), number_string(ID, Name), select_sp_data(ID), nl)
          ),
    unload_file('../../out/SP/objects_and_annotations/football.pl').

