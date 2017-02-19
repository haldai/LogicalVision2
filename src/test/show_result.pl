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
%% visualization of the results

:- discontiguous object/2.
:- discontiguous clock_angle/4.

:- ensure_loaded(['test3.pl']).

object_points(Img, elps(Cen, Param), Pts):-
    size_2d(Img, W, H),
    ellipse_points_2d(Cen, Param, [W, H], Pts).
object_points(Img, circle(Cen, Rad), Pts):-
    size_2d(Img, W, H),
    circle_points_2d(Cen, Rad, [W, H], Pts).

show_protist_train(Turn, Name):-
    Path = '../../out/protist/train/',
    Img_Path = '../../data/protist/train/',
    atomic_concat(Name, '.pl', File),
    atomic_concat(Path, File, File_Path),
    atomic_concat(Name, '.jpg', Img_File),
    atomic_concat(Img_Path, Img_File, Img_File_Path),
    consult(File_Path),
    load_img(Img_File_Path, Img),
    object(Turn, Obj),
    object_points(Img, Obj, Pts),
    draw_points_2d(Img, Pts, red),
    showimg_win(Img, Name),
    save_img(Img, '../../tmp/tmp.png'),
    release_img(Img),
    unload_file(File_Path), !.

show_protist_test(Turn, Name):-
    Path = '../../out/protists/test_1/',
    Img_Path = '../../data/protists/',
    atomic_concat(Name, '.pl', File),
    atomic_concat(Path, File, File_Path),
    atomic_concat(Name, '.jpg', Img_File),
    atomic_concat(Img_Path, Img_File, Img_File_Path),
    consult(File_Path),
    load_img(Img_File_Path, Img),
    object(Turn, Obj),
    object_points(Img, Obj, Pts),
    draw_points_2d(Img, Pts, red),
    showimg_win(Img, Name),
    save_img(Img, '../../tmp/tmp.png'),
    release_img(Img),
    unload_file(File_Path), !.

show_moon_train(Turn, Name):-
    Path = '../../out/moon/train/',
    Img_Path = '../../data/moon/train/',
    atomic_concat(Name, '.pl', File),
    atomic_concat(Path, File, File_Path),
    atomic_concat(Name, '.jpg', Img_File),
    atomic_concat(Img_Path, Img_File, Img_File_Path),
    consult(File_Path),
    load_img(Img_File_Path, Img),
    object(Turn, Obj),
    object_points(Img, Obj, Pts),
    draw_points_2d(Img, Pts, red),
    showimg_win(Img, Name),
    save_img(Img, '../../tmp/tmp.png'),
    release_img(Img),
    unload_file(File_Path), !.

show_moon_test(Turn, Name):-
    Path = '../../out/moon/test/',
    Img_Path = '../../data/moons/',
    atomic_concat(Name, '.pl', File),
    atomic_concat(Path, File, File_Path),
    atomic_concat(Name, '.jpg', Img_File),
    atomic_concat(Img_Path, Img_File, Img_File_Path),
    consult(File_Path),
    load_img(Img_File_Path, Img),
    object(Turn, Obj),
    object_points(Img, Obj, Pts),
    draw_points_2d(Img, Pts, red),
    showimg_win(Img, Name),
    save_img(Img, '../../tmp/tmp.png'),
    release_img(Img),
    unload_file(File_Path), !.
