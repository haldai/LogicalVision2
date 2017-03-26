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


test_sp(Img):-
    test_write_start('test super pixel'),
    time(create_superpixels(Img, [0, 20, 10, 5, 25], SP)),
    % save_superpixels(SP, '../../out/SP/20/tmp.csv'),
    num_superpixels(SP, Num), write('Num superpixels: '), write(Num), nl,    
    show_superpixels(Img, SP),
    get_sps_pixels(SP, [], Pts),
    clone_img(Img, Img2),
    draw_points_2d(Img2, Pts, red),
    showimg_win(Img2, 'debug'),
    %save_img(Img2, '../../tmp/tmp.png'),
    release_img(Img2),
    release_sp(SP),
    test_write_done.

test_load_img_4(A):-
    test_write_start('load image'),
    %load_img('../../data/MobileRobotAndBall1/raw_images/6010.jpg', A),
    load_img('../../data/MobileRobotAndBall1/raw_images/320.jpg', A),
    size_2d(A, X, Y),
    write('W x H: '),
    write(X), write(' x '), write(Y), nl,
    test_write_done.

test_sample_in_sp(Img):-
    test_write_start('test line sampling inside of super pixel'),
    % time(create_superpixels(Img, [1, 20, 10, 5, 25], _)),
    load_superpixels('../../out/SP/Statistical/320.csv', SP),
    IDs = [66, 69],

    sps_ball_vertical_line_color_trans(Img, SP, IDs, SegsPts, Colors),
    print_list_ln(Colors),
    append(SegsPts, Pts),
    print_list(Pts),

    show_superpixels(Img, SP),
    clone_img(Img, Img2),
    get_sps_pixels(SP, IDs, SPts),
    draw_points_2d(Img2, SPts, red),
    draw_points_2d(Img2, Pts, blue),
    showimg_win(Img2, 'debug'),
    release_img(Img2),

    release_sp(SP),
    test_write_done.

test_sp_line_pts_labels(Img):-
    test_write_start('sample line superpixel labels'),
    time(create_superpixels(Img, [0, 20, 10, 3, 25], SP)),
    sp_line_pts_labels(SP, [100, 100], [1, 1], Labels1),
    list_to_set(Labels1, Labels),
    print_list(Labels),
    release_sp(SP),
    test_write_done.

test_view_directory:-
    Dir = '../../data/MobileRobotAndBall1/raw_images/',
    directory_files(Dir, Files),
    remove_files_extension(Files, Names),
    forall(member(Name, Names),
           (writeln(Name),
            number_string(ID, Name),
            atomic_list_concat(['../../out/SP/final', 'Relational'], '/', Path),
            atomic_list_concat(['../../out/SP/final', 'Statistical'],
                               '/', SPath),
            atomic_concat(ID, '.csv', SPFile),
            % atomic_concat(ID, '.txt', SLFile),
            atomic_list_concat([SPath, SPFile], '/', SPPath),
            atomic_concat(ID, '_labels.pl', LFile),
            % atomic_concat(ID, '_bk.pl', BFile),
            atomic_list_concat([Path, LFile], '/', LPath),
            load_superpixels(SPPath, SP),
            consult(LPath),
            ball_sp(ID, SPs),
            unload_file(LPath),            
            atomic_list_concat([Dir, ID], '/', ImgName),
            atomic_concat(ImgName, '.jpg', ImgPath),
            load_img(ImgPath, Img),
            show_superpixels(Img, SP),
            get_sps_pixels(SP, SPs, Pts),            
            clone_img(Img, Img2),
            draw_points_2d(Img2, Pts, red),
            showimg_win(Img2, Name),
            release_sp(SP),
            release_img(Img),
            release_img(Img2),
            nl)
          ).

test_fit_ball(ImgID, SP_IDs):-
    test_write_start('test fit ball'),
    %fit_football(ImgID, SP_IDs, Circle),
    %writeln(Circle),
    find_ball(ImgID, SP_IDs, 5),
    test_write_done.

test_fit_ball_directory:-
    Dir = '../../data/MobileRobotAndBall1/raw_images/',
    directory_files(Dir, Files),
    remove_files_extension(Files, Names),
    forall(member(Name, Names),
           (writeln('~~~~~~~~~~~~~'),
            writeln(Name),
            number_string(ID, Name),
            atomic_list_concat(['../../out/SP', 'Relational'], '/', Path),
            atomic_concat(ID, '_labels.pl', LFile),
            atomic_list_concat([Path, LFile], '/', LPath),
            consult(LPath),
            ball_sp(ID, SP_Strings),
            (SP_Strings \= [],
             unload_file(LPath),
             findall(SPID,
                     (member(SP, SP_Strings), split_string(SP, "_", "", SP_L),
                      last(SP_L, SP_LE), number_string(SPID, SP_LE)),
                     SPs),
             (find_ball(ID, SPs, 5) -> true; write('+++')), !);
            (writeln('No ball')),
            !,
            writeln('~~~~~~~~~~~~~'))
          ).

test_main_4:-
    test_load_img_4(Img),
    %test_sp(Img),
    %test_sample_in_sp(Img),
    %test_sp_line_pts_labels(Img),
    %test_view_directory,
    %test_fit_ball(1480, [364, 355]),
    test_fit_ball_directory,
    test_rel_img(Img).
