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

labeling_proportion(1/5).

test_load_img_4(A):-
    test_write_start('load image'),
    %load_img('../../data/MobileRobotAndBall1/raw_images/1485.jpg', A),
    load_img('../../data/MobileRobotAndBall1/raw_images/65.jpg', A),
    size_2d(A, X, Y),
    write('W x H: '),
    write(X), write(' x '), write(Y), nl,
    test_write_done.

test_sp_lsc(Img):-
    test_write_start('test super pixel'),
    time(create_superpixels(Img, [0, 20, 10, 3, 25], SP)),
    save_superpixels(SP, '../../out/SP/20/tmp.csv'),
    num_superpixels(SP, Num), write('Num superpixels: '), write(Num), nl,    
    show_superpixels(Img, SP),
    get_sps_pixels(SP, [0, 27], Pts),
    clone_img(Img, Img2),
    draw_points_2d(Img2, Pts, red),
    showimg_win(Img2, 'debug'),
    release_img(Img2),
    release_sp(SP),
    test_write_done.

test_sample_in_sp(Img):-
    test_write_start('test line sampling inside of super pixel'),
    % time(create_superpixels(Img, [1, 20, 10, 5, 25], _)),
    time(load_superpixels('../../out/SP/20/Statistical/65.csv', SP)), 
    IDs = [146, 144, 134], length(IDs, L),
    init_vec(L, 10, Sample_times),
    time(concurrent_maplist(sp_rand_lines_color_trans(Img, SP), IDs, Sample_times, Colors)),
    print_list_ln(Colors),
    time(maplist(sp_rand_lines_color_trans(Img, SP), IDs, Sample_times, Colors1)),
    print_list_ln(Colors1),
    /*
    append(SegsPts, Pts),    
    show_superpixels(Img, SP),
    clone_img(Img, Img2),
    get_sps_pixels(SP, IDs, SPts),
    draw_points_2d(Img2, SPts, red),
    draw_points_2d(Img2, Pts, blue),
    showimg_win(Img2, 'debug'),
    release_img(Img2),
    */
    release_sp(SP),
    test_write_done.

test_gen_sp_extra_data(ID, SPSize):-
    Dir = '../../data/MobileRobotAndBall1/raw_images/',
    atomic_concat('../../out/SP/', SPSize, ODir0),
    atomic_concat(ODir0, '/', ODirX), atomic_concat(ODirX, ID, PathX),
    atomic_concat(ODir0, '/Statistical/', ODir),
    atomic_concat(Dir, ID, Path1), atomic_concat(Path1, '.jpg', ImgPath),
    atomic_concat(ODir, ID, Path2), atomic_concat(Path2, '.csv', CSVPath),
    load_img(ImgPath, Img),
    time(load_superpixels(CSVPath, SP)),
    num_superpixels(SP, SPNum), SPNum1 is SPNum - 1,
    findall(I, between(0, SPNum1, I), Is),
    length(Is, Len), init_vec(Len, 10, Repeat),
    writeln("Sampling: "),
    time(concurrent_maplist(sp_rand_lines_color_trans(Img, SP), Is, Repeat, Colors)),

    atomic_concat(PathX, '_extra_bk.pl', PlPath),
    tell(PlPath),
    forall(between(0, SPNum1, X),
           (nth0(X, Is, SP_ID),
            nth0(X, Colors, SP_Color),
            write(sp_sampled_color_trans(SP_ID, SP_Color)), write(".\n")
           )
          ),
    told,
    release_sp(SP),
    release_img(Img).

gen_sp_label(ID, SP, Pos_fb, Neg_fb, Pos_nao, Neg_nao, Pos_gp, Neg_gp):-
    ensure_loaded(['../../data/MobileRobotAndBall1/football.pl',
                   '../../data/MobileRobotAndBall1/nao.pl',
                   '../../data/MobileRobotAndBall1/goal_post.pl']),
    num_superpixels(SP, SPNum), N is SPNum - 1,
    ((football(ID, Box_fb) ->
          sp_labeling(SP, Box_fb, N, Pos_fb, Neg_fb);
      (Pos_fb = [], findall(X, between(0, N, X), Neg_fb)))),
    ((nao(ID, Box_nao) ->
          sp_labeling(SP, Box_nao, N, Pos_nao, Neg_nao);
      (Pos_nao = [], findall(X, between(0, N, X), Neg_nao)))),
    ((goal_post(ID, Box_gp) ->
          sp_labeling(SP, Box_gp, N, Pos_gp, Neg_gp);
      (Pos_gp = [], findall(X, between(0, N, X), Neg_gp)))).

sp_labeling(_, _, -1, [], []):-
    !.
sp_labeling(SP, Box, N, [N | Pos], Neg):-
    get_sp_pixels(SP, N, Pts),
    length(Pts, Total),
    points_in_box(Box, Pts, Num),
    rect_area(Box, Area),
    %write(N), write(': '), write(Num), write(','), write(Total), write(','), write(Area),
    labeling_proportion(P),
    (Num/Total > P; Num/Area > P), !,
    %write(' -- *'), nl,
    N1 is N - 1,
    sp_labeling(SP, Box, N1, Pos, Neg), !.
sp_labeling(SP, Box, N, Pos, [N | Neg]):-
    %nl,
    N1 is N - 1,
    sp_labeling(SP, Box, N1, Pos, Neg).

process_directory_2:-
    Dir = '../../data/MobileRobotAndBall1/raw_images/',
    directory_files(Dir, Files),
    remove_files_extension(Files, Names),
    forall(member(Name, Names),
           (number_string(ID, Name),
            write("Processing "), writeln(ID),
            gen_sp_extra_data(ID, 10),
            gen_sp_extra_data(ID, 20),
            gen_sp_extra_data(ID, 30).
           )
          ).

test_main_4:-
    test_load_img_4(Img),
    %test_sp_lsc(Img),
    %test_sample_in_sp(Img),
    test_gen_sp_extra_data(4555, 10),
    test_rel_img(Img).
