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
/* Test module - 4
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

:- ensure_loaded(['test3.pl',
                  '../abduce/bk_football.pl']).

gen_sp_extra_data(ID):-
    Dir = '../../data/MobileRobotAndBall1/raw_images',
    ODir = '../../out/SP',
    atomic_list_concat([Dir, ID], '/', ImgName),
    atomic_concat(ImgName, '.jpg', ImgPath),
    atomic_list_concat([ODir, 'Statistical', ID], '/', CSVName),
    atomic_concat(CSVName, '.csv', CSVPath),
    load_img(ImgPath, Img),
    time(load_superpixels(CSVPath, SP)),
    num_superpixels(SP, SPNum), SPNum1 is SPNum - 1,
    findall(I, between(0, SPNum1, I), Is),
    length(Is, Len), init_vec(Len, 10, Repeat),
    writeln("Sampling: "),
    time(concurrent_maplist(sp_ball_rand_lines_color_trans(Img, SP), Is, Repeat, Colors)),
    atomic_list_concat([ODir, 'bk_color_trans', ID], '/', PlName),
    atomic_concat(PlName, '_bk_color_trans.pl', PlPath),
    tell(PlPath),
    forall(between(0, SPNum1, X),
           (nth0(X, Is, SP_ID),
            nth0(X, Colors, SP_Color),
            write(sp_color_trans(SP_ID, SP_Color)), write(".\n")
           )
          ),
    told,
    release_sp(SP),
    release_img(Img).

gen_sp_extra_data_2(ID):-
    Dir = '../../data/MobileRobotAndBall1/raw_images',
    ODir = '../../out/SP',
    atomic_list_concat([Dir, ID], '/', ImgName),
    atomic_concat(ImgName, '.jpg', ImgPath),
    atomic_list_concat([ODir, 'Statistical', ID], '/', CSVName),
    atomic_concat(CSVName, '.csv', CSVPath),
    load_img(ImgPath, Img),
    time(load_superpixels(CSVPath, SP)),
    num_superpixels(SP, SPNum), SPNum1 is SPNum - 1,
    findall(I, between(0, SPNum1, I), Is),
    length(Is, Len), init_vec(Len, 10, Repeat),
    writeln("Sampling: "),
    time(concurrent_maplist(sp_ball_vertical_lines_color_trans(Img, SP), Is, Repeat, Colors)),
    atomic_list_concat([ODir, 'bk_color_trans_vertical', ID], '/', PlName),
    atomic_concat(PlName, '_bk_color_trans_vertical.pl', PlPath),
    tell(PlPath),
    forall(between(0, SPNum1, X),
           (nth0(X, Is, SP_ID),
            nth0(X, Colors, SP_Color),
            write(sp_color_trans_vertical(SP_ID, SP_Color)), write(".\n")
           )
          ),
    told,
    release_sp(SP),
    release_img(Img).

process_directory:-
    Dir = '../../data/MobileRobotAndBall1/raw_images/',
    directory_files(Dir, Files),
    remove_files_extension(Files, Names),
    forall(member(Name, Names),
           (number_string(ID, Name),
            write("Processing "), writeln(ID),
            gen_sp_extra_data(ID),
            gen_sp_extra_data_2(ID)
           )
          ).
