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

gen_sp_data(ID, Type, SPSize):-
    Dir = '../../data/MobileRobotAndBall1/raw_images/',
    atomic_concat('../../out/SP/', Type, ODir0),
    atomic_concat(ODir0, '/', ODir0_),
    atomic_concat(ODir0_, SPSize, ODir0__),
    atomic_concat(ODir0__, '/', ODir),
    atomic_concat(Dir, ID, Path1), atomic_concat(Path1, '.jpg', ImgPath),
    atomic_concat(ODir, 'Statistical/', ODirS),  
    atomic_concat(ODirS, ID, Path2), atomic_concat(Path2, '.csv', CSVPath),
    load_img(ImgPath, Img),
    create_superpixels(Img, [Type, SPSize, 10, 5, 25], SP),
    num_superpixels(SP, SPNum),
    write('Size = '), write(SPSize), write(' ----- '),
    write('Num superpixels: '), write(SPNum), nl,
    save_superpixels(SP, CSVPath),
    % labeling of superpixels
    % gen_sp_label(ID, SP, Pos_fb, Neg_fb, Pos_nao, Neg_nao, Pos_gp, Neg_gp),
    gen_sp_label(ID, SP, Pos_fb, _, _, _, _, _),
    writeln('ball Pos:'),
    print_list(Pos_fb),
    /*%% debug
    show_superpixels(Img, SP),
    get_sps_pixels(SP, Pos_fb, Pts),
    clone_img(Img, Img2),
    draw_points_2d(Img2, Pts, red),
    showimg_win(Img2, 'debug'),
    save_img(Img2, 'tmp.png'),
    release_img(Img2),
    %% debug end*/
    atomic_concat(ODirS, ID, Path2_), atomic_concat(Path2_, '.txt', CSVPath2),
    atomic_concat(ODir, 'Relational/', ODirR), atomic_concat(ODirR, ID, Path3_),
    atomic_concat(Path3_, '_labels', Path3), atomic_concat(Path3, '.pl', PlPath),
    Pl_SP_Term = num_superpixels(ID, SPNum),
    Pl_Ball_Term = ball_sp(ID, Pos_fb),
    Pl_Nao_Term = nao_sp(ID, Pos_nao),
    Pl_GP_Term = gp_sp(ID, Pos_gp),
    tell(PlPath),
    write(Pl_Ball_Term), writeln('.'),
    write(Pl_Nao_Term), writeln('.'),
    write(Pl_GP_Term), writeln('.'),
    told,
    tell(CSVPath2),
    write('num_superpixels :'), write('\t'), writeln(SPNum),
    write('ball_superpixels:'),write('\t'), writeln(Pos_fb),
    write('nao_superpixels :'),write('\t'), writeln(Pos_nao),
    write('gp_superpixels  :'),write('\t'), writeln(Pos_gp),
    told,

    %% Background knowledge for prolog
    %% write background file
    atomic_concat(Path3_, '_bk', Path4), atomic_concat(Path4, '.pl', PlPath2),
    tell(PlPath2),
    writeln('%% num_superpixels(ImgID,Num): Number of all superpixels. They are named from 0 to N-1.'), nl,
    write(Pl_SP_Term), writeln('.'),
    % adjacency
    get_sp_all_adj_pairs(SP, Pairs),    
    nl, writeln('%% next_to(SuperPixelID1, SuperPixelID2): Adjacency.'), nl,
    forall(member([S1, S2], Pairs), (write(next_to(S1, S2)), writeln('.'))),
    MaxLabel is SPNum - 1,
    % location
    nl, writeln('%% sp_location(SuperPixelID, [X, Y]): Superpixel locations.'), nl,
    forall(between(0, MaxLabel, L),
           (get_sp_position(SP, L, [SX, SY]),
            write(sp_location(L, [SX, SY])), writeln('.')
           )
          ),
    % size
    nl, writeln('%% sp_size(SuperPixelID, NumOfPixels): Superpixel sizes.'), nl,
    forall(between(0, MaxLabel, L),
           (get_sp_pixels(SP, L, PXs),
            length(PXs, Sz),
            write(sp_size(L, Sz)), writeln('.')
           )
          ),
    % color
    nl, writeln('%% white/black/grey/green(SuperPixelID, Proportion): Color proportion inside the superpixel.'), nl,
    forall(between(0, MaxLabel, L),
           (sp_colors(Img, SP, L, WW, BB, EE, GG),
            write(WW), writeln('.'),
            write(BB), writeln('.'),
            write(EE), writeln('.'),
            write(GG), writeln('.')
           )
          ),
    told,
    /*
    writeln('ball Neg:'),
    print_list(Neg_fb),
    writeln('nao Pos:'),    
    print_list(Pos_nao),
    writeln('nao Neg:'),    
    print_list(Neg_nao),
    writeln('gp Pos:'),    
    print_list(Pos_gp),
    writeln('gp Neg:'),    
    print_list(Neg_gp),
    */
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
    %write(N), write(': '), write(Num), write(','), write(Total), write(','), write(Area),
    labeling_proportion(P),
    % (Num/Total > P; Num/Area > P), !,
    Num/Total > P,
    %write(' -- *'), nl,
    centroid_int(Pts, Cen),
    in_box(Box, Cen),
    N1 is N - 1,
    sp_labeling(SP, Box, N1, Pos, Neg), !.
sp_labeling(SP, Box, N, Pos, [N | Neg]):-
    %nl,
    N1 is N - 1,
    sp_labeling(SP, Box, N1, Pos, Neg).

sp_colors(Img, SP, L, white(L, W), black(L, B), grey(L, E), green(L, G)):-
    get_sp_pixels(SP, L, Pts),
    pts_color_2d(Img, Pts, LABs), colors(LABs, Clrs),
    length(Pts, Tot),
    findall(X, (member(X, Clrs), member(white, X)), WW),
    length(WW, LW), W is LW/Tot,
    findall(X, (member(X, Clrs), member(black, X)), BB),
    length(BB, LB), B is LB/Tot,
    findall(X, (member(X, Clrs), member(gray, X)), EE),
    length(EE, LE), E is LE/Tot,
    findall(X, (member(X, Clrs), member(green, X)), GG),
    length(GG, LG), G is LG/Tot.

process_directory:-
    Dir = '../../data/MobileRobotAndBall1/raw_images/',
    directory_files(Dir, Files),
    remove_files_extension(Files, Names),
    forall(member(Name, Names),
           (number_string(ID, Name),
            write("============= "), write(ID), writeln(" ============="),
            /*
            gen_sp_data(ID, 2, 30),
            gen_sp_data(ID, 2, 25),
            gen_sp_data(ID, 2, 20),
            gen_sp_data(ID, 2, 15),
            gen_sp_data(ID, 2, 10),
            */
            writeln('***** SLIC ******'),
            concurrent_maplist(gen_sp_data(ID, 0), [30, 25, 20, 15, 10]),
            writeln('***** SLICO ******'),
            concurrent_maplist(gen_sp_data(ID, 1), [30, 25, 20, 15, 10]),
            writeln('***** MSLIC ******'),
            concurrent_maplist(gen_sp_data(ID, 2), [30, 25, 20, 15, 10]),
            nl
           )
          ).

