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

:- ensure_loaded(['test3.pl']).

test_load_img_4(A):-
    test_write_start('load image'),
    %load_img('../../data/MobileRobotAndBall1/raw_images/1485.jpg', A),
    load_img('../../data/MobileRobotAndBall1/raw_images/65.jpg', A),
    size_2d(A, X, Y),
    write('W x H: '),
    write(X), write(' x '), write(Y), nl,
    test_write_done.

test_sp_lsc(Img):-
    test_write_start('test super pixel LSC'),
    time(create_superpixels(Img, [0, 10, 10, 2, 25], SP)),
    show_superpixels(Img, SP),
    release_sp(SP),
    test_write_done.

test_main_4:-
    test_load_img_4(Img),
    test_sp_lsc(Img),
    test_rel_img(Img).
