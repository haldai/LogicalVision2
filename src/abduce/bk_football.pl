%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% hypothesis-testing for finding football
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. find green->not_green (GNG) and not_green->green (NGG) points
% 2. inside points are all black & white
% 2.1 pattern: black in white
% 3. make conjecture this is a football
% 4. test it by sampling other lines near current line

% determine if a segment only contains black and white
bw_seg_2d(Img, [Start, End]):-
    sample_line_seg_color_2d(Img, Start, End, _, LABs),
    % DEBUG
    write([Start, End]), write(': '),
    colors(LABs, Colors), print_list(Colors),
    % points inside of this seg are all black and whites
    findall(C,
            (member(C, LABs),
             (color(C, only(white));
              color(C, only(black))
             )
            ),
            BW_Pts),
    length(LABs, L0),    
    length(BW_Pts, L1),
    L1/L0 >= 0.8.
    
% sample a line and get green/non-green points
sample_line_GNG_points_2d(Img, [Pt, Dir], GNGs):-
    sample_line_color_change_2d(Img, [Pt, Dir], green, not(green), P1),
    sample_line_color_change_2d(Img, [Pt, Dir], not(green), green, P2),
    append(P1, P2, GNGs_),
    sort(GNGs_, GNGs).

% get GNG segs from a list of spiral segs
get_GNG_seg_lists(Img, Segs, Re):-
    get_GNG_seg_lists(Img, Segs, [], Re), !.
get_GNG_seg_lists(_, [], Re, Re):-
    !.
get_GNG_seg_lists(Img, [Seg | Segs], Temp, Re):-
    get_GNG_seg(Img, Seg, P),
    append(Temp, P, Temp1),
    get_GNG_seg_lists(Img, Segs, Temp1, Re).

% get segments that from GNG to NGG points
get_GNG_seg(Img, Pts, Re):-
    length(Pts, N),
    get_GNG_seg(Img, Pts, 1, N, Re).

get_GNG_seg(_, _, M, N, []):-
    M >= N, !.
get_GNG_seg(Img, Pts, I, N, [R | Re]):-
    (gng_point(Img, Pts, I);
     (I == 1, Pts = [P | _],
      pt_color_2d(Img, P, C), color(C, not(green)))),
    !,
    insert_GNG_seg(Img, Pts, I, N, RI, R),
    I1 is RI + 1,
    get_GNG_seg(Img, Pts, I1, N, Re),
    !.
get_GNG_seg(Img, Pts, I, N, Re):-
    I1 is I + 1,
    get_GNG_seg(Img, Pts, I1, N, Re).

insert_GNG_seg(Img, Pts, I, _, I, []):-
    ngg_point(Img, Pts, I), !.
insert_GNG_seg(_, _, I, N, N, []):-
    I > N,
    !.
insert_GNG_seg(Img, Pts, I, N, RI, [P | Re]):-
    nth1(I, Pts, P),
    I1 is I + 1,
    insert_GNG_seg(Img, Pts, I1, N, RI, Re).

gng_point(Img, Pts, I):-
    color_chg_idx(Img, Pts, I, green, not(green)).
ngg_point(Img, Pts, I):-
    color_chg_idx(Img, Pts, I, not(green), green).

% make training instances
gen_ball_instances(_, [], [], []):-
    !.
gen_ball_instances(Box, [S | Segs], [S | Pos], Neg):-
    length(S, N), N > 0, points_in_box(Box, S, M),
    % write(M), write('/'), write(N), nl,
    M/N > 0, % 8e-1,
    gen_ball_instances(Box, Segs, Pos, Neg), !.
gen_ball_instances(Box, [[] | Segs], Pos, Neg):-
    gen_ball_instances(Box, Segs, Pos, Neg), !.
gen_ball_instances(Box, [S | Segs], Pos, [S | Neg]):-
    gen_ball_instances(Box, Segs, Pos, Neg).

% generate training data
gen_ball_train(Path, Pos_exs, Neg_exs):-
    football(Path, Box),
    load_img(Path, Img), size_2d(Img, W, H),
    write('Sampling: '), time(rand_spiral_sampling([W, H], [0, 0.5], 20, Spiral_Segs)),
    write('Filtering: '), time(get_GNG_seg_lists(Img, Spiral_Segs, GNGSegs)),
    % append(GNGSegs, PPPP),
    % clone_img(Img, Img1),
    % draw_points_2d(Img1, PPPP, red),
    % showimg_win(Img1, Path),
    % release_img(Img1),
    write('Generating: '), time(gen_ball_instances(Box, GNGSegs, Pos, Neg)),
    findall(T,
            (member(X, Pos),
             atomics_to_string(["\'", Path, "\'"], PathStr),
             T =.. ['ballseg', PathStr, X]),
            Pos_list
           ),
    findall(T,
            (member(X, Neg),
             atomics_to_string(["\'", Path, "\'"], PathStr),
             T =.. ['ballseg', PathStr, X]),
            Neg_list
           ),
    Pos_exs =.. ['pos', Pos_list],
    Neg_exs =.. ['neg', Neg_list],
    release_img(Img).

gen_ball_train_pts:-
    ['/home/daiwz/Myprojects/LogicalVision2/data/MobileRobotAndBall1/football.pl'],
    OutPath = '/home/daiwz/Myprojects/LogicalVision2/data/ball_train/',
    forall(gen_ball_train(Path, Pos_exs, Neg_exs),
           (split_string(Path, '/', '', L), last(L, Name),
            split_string(Name, '.', '', NL), NL = [Name0, _],
            atomics_to_string([OutPath, Name0, '_train', '.pl'], OutFile),
            open(OutFile, write, Out),
            write(Out, Pos_exs), write(Out, '.'), write(Out, '\n'),
            writeln(Out, Neg_exs), write(Out, '.'), write(Out, '\n'),
            close(Out)
           )
          ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% generate sequences for Prof Muggleton
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% make training instances
gen_ball_instances_2(_, [], [], []):-
    !.
gen_ball_instances_2(Box, [S | Segs], [S | Pos], Neg):-
    length(S, N), N > 0, points_in_box(Box, S, M),
    % write(M), write('/'), write(N), nl,
    M/N > 0, % 8e-1,
    gen_ball_instances_2(Box, Segs, Pos, Neg), !.
gen_ball_instances_2(Box, [[] | Segs], Pos, Neg):-
    gen_ball_instances_2(Box, Segs, Pos, Neg), !.
gen_ball_instances_2(Box, [S | Segs], Pos, [S | Neg]):-
    gen_ball_instances_2(Box, Segs, Pos, Neg).
    
% new training
gen_ball_train_2(Path, Pos_exs, Neg_exs):-
    football(Path, Box),
    load_img(Path, Img), size_2d(Img, W, H),
    % get arcs
    write('Sampling: '), time(rand_spiral_sampling([W, H], [0, 0.5], 10, Arcs)),
    write('Generating: '), time(gen_ball_instances_2(Box, Arcs, Pos_, Neg_)),
    % morph to transition
    get_color_transitions(Img, Pos_, Pos),
    get_color_transitions(Img, Neg_, Neg),
    findall(T,
            (member(X, Pos),
             %atomics_to_string(["\'", Path, "\'"], PathStr),
             T =.. ['ballseg', X]),
            Pos_list
           ),
    findall(T,
            (member(X, Neg),
             %atomics_to_string(["\'", Path, "\'"], PathStr),
             T =.. ['ballseg', X]),
            Neg_list
           ),
    Pos_exs =.. ['pos', Pos_list],
    Neg_exs =.. ['neg', Neg_list],
    release_img(Img).

% get color transition
get_color_transitions(_, [], []):-
    !.
get_color_transitions(Img, [A | Arcs], [Tr | Trans]):-
    pts_color_2d(Img, A, LABs),
    color_transition(LABs, Tr, nil),
    get_color_transitions(Img, Arcs, Trans).
color_transition([], [], _):-!.
color_transition([L | LABs], [C | Cs], Pre):-
    not(color(L, Pre)),
    color(L, C), !,
    color_transition(LABs, Cs, C).
color_transition([_ | LABs], Cs, Pre):-
    color_transition(LABs, Cs, Pre).

gen_ball_train_2:-
    ['/home/daiwz/Myprojects/LogicalVision2/data/MobileRobotAndBall1/football.pl'],
    OutPath = '/home/daiwz/Myprojects/LogicalVision2/data/ball_train_2/',
    forall(gen_ball_train_2(Path, Pos_exs, Neg_exs),
           (split_string(Path, '/', '', L), last(L, Name),
            split_string(Name, '.', '', NL), NL = [Name0, _],
            atomics_to_string([OutPath, Name0, '_train', '.pl'], OutFile),
            open(OutFile, write, Out),
            write(Out, "%Following are positive examples.\n"),
            write(Out, Pos_exs), write(Out, '.'), write(Out, '\n'),
            write(Out, "%Following are negative examples.\n"),
            writeln(Out, Neg_exs), write(Out, '.'), write(Out, '\n'),
            close(Out)
           )
          ).

