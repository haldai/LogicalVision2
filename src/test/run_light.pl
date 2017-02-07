/* Run Logical Vision for highlight calculation
 * =============================================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

:- ensure_loaded(['../io/plio.pl',
                  '../sampling/plsampling.pl',
                  '../drawing/pldraw.pl',
                  '../stats/plstats.pl',
                  '../abduce/plabduce.pl',
                  '../abduce/bk_light.pl',
                  '../abduce/bk_object.pl',
                  '../abduce/bk_ellipse.pl',
                  '../abduce/bk_polygon.pl',
                  '../abduce/bk_football.pl',
                  '../stats/ball_region.pl',
                  '../sampling/plregion.pl',
                  '../sampling/plpoints.pl',
                  '../utils/utils.pl',
                  '../learning/pllearning.pl']).

run_protist(Num):-
    Path = '../../data/protist/',
    Out_Path = '../../out/protist/',
    atomic_concat(Path, 'train/', Train_Path),
    atomic_concat(Path, 'test/', Test_Path),
    atomic_concat(Out_Path, 'train/', Out_Train_Path),
    atomic_concat(Out_Path, 'test/', Out_Test_Path),
    directory_files(Train_Path, Train_Files),
    directory_files(Test_Path, Test_Files),
    load_model_svm('../../tmp/SVM_Protist.model', Model),
    %process_protist(Model, Train_Files, Train_Path, Out_Train_Path, Num, 1),
    process_protist(Model, Test_Files, Test_Path, Out_Test_Path, Num, 1),
    release_model_svm(Model).
 
process_protist(_, [], _, _, _, _):-
    !.
process_protist(Model, [File | Files], Path, Out_Path, Num, Count):-
    %% debug
    writeln(process_protist(Model, [File | Files], Path, Out_Path, Num, Count)),
    File \= '.', File \= '..',
    split_string(File, '.', '', [Name, "jpg"]),
    atomic_concat(Name, ".pl", Out_File),
    atomic_concat(Path, File, File_Path),
    atomic_concat(Out_Path, Out_File, Out_File_Path),
    % load image
    load_img(File_Path, Img),
    write('Abduce protist\t'), write("#"), write(Num),
    write('\t'), write(Name), write('\t'), writeln(Count),
    ((time(abduce_object_elps(Img, Model, [], Obj, 0)),
      get_largest_contrast_angle(Img, Obj, Ang));
     (Obj = 'nil', Ang = 'nil')
    ), !,
    append(Out_File_Path),
    write(object(Num, Obj)), writeln("."),
    write(clock_angle(Num, part1, part2, Ang)), writeln("."),
    told,
    release_img(Img),
    Count1 is Count + 1,
    process_protist(Model, Files, Path, Out_Path, Num, Count1),
    !.
process_protist(Model, [_ | Files], Path, Out_Path, Num, Count):-
    process_protist(Model, Files, Path, Out_Path, Num, Count), !.

run_moon(Num):-
    Path = '../../data/moon/',
    Out_Path = '../../out/moon/',
    atomic_concat(Path, 'train/', Train_Path),
    atomic_concat(Path, 'test/', Test_Path),
    atomic_concat(Out_Path, 'train/', Out_Train_Path),
    atomic_concat(Out_Path, 'test/', Out_Test_Path),
    directory_files(Train_Path, Train_Files),
    directory_files(Test_Path, Test_Files),
    load_model_svm('../../tmp/SVM_Moon.model', Model),
    process_moon(Model, Train_Files, Train_Path, Out_Train_Path, Num, 1),
    process_moon(Model, Test_Files, Test_Path, Out_Test_Path, Num, 1),
    release_model_svm(Model).

process_moon(_, [], _, _, _, _):-
    !.
process_moon(Model, [File | Files], Path, Out_Path, Num, Count):-
    File \= '.', File \= '..',
    split_string(File, '.', '', [Name, "jpg"]),
    atomic_concat(Name, ".pl", Out_File),
    atomic_concat(Path, File, File_Path),
    atomic_concat(Out_Path, Out_File, Out_File_Path),
    % load image
    load_img(File_Path, Img),
    write('Abduce moon\t'), write("#"), write(Num),
    write('\t'), write(Name), write('\t'), writeln(Count),
    ((time(abduce_object_elps(Img, Model, [], Obj)),
     get_largest_contrast_angle(Img, Obj, Ang));
     (Obj = 'nil', Ang = 'nil')
    ), !,
    append(Out_File_Path),
    write(object(Num, Obj)), writeln("."),
    write(clock_angle(Num, part1, part2, Ang)), writeln("."),
    told,
    release_img(Img),
    Count1 is Count + 1,
    process_moon(Model, Files, Path, Out_Path, Num, Count1),
    !.
process_moon(Model, [_ | Files], Path, Out_Path, Num, Count):-
    process_moon(Model, Files, Path, Out_Path, Num, Count), !.

process_moon_circle(_, [], _, _, _, _):-
    writeln('Reset!'),
    !.
process_moon_circle(Model, [File | Files], Path, Out_Path, Num, Count):-
    writeln(process_moon_circle(Model, [File | Files], Path, Out_Path, Num, Count)),
    File \= '.', File \= '..',
    split_string(File, '.', '', [Name, "jpg"]),
    atomic_concat(Name, ".pl", Out_File),
    atomic_concat(Path, File, File_Path),
    atomic_concat(Out_Path, Out_File, Out_File_Path),
    % load image
    load_img(File_Path, Img),
    write('Abduce moon\t'), write("#"), write(Num),
    write('\t'), write(Name), write('\t'), writeln(Count),
    ((time(abduce_object_circle(Img, Model, [], Obj, 0)),
     get_largest_contrast_angle(Img, Obj, Ang));
     (Obj = 'nil', Ang = 'nil')
    ), !,
    writeln(Obj),
    append(Out_File_Path),
    write(object(Num, Obj)), writeln("."),
    write(clock_angle(Num, part1, part2, Ang)), writeln("."),
    told,
    release_img(Img),
    Count1 is Count + 1,
    process_moon_circle(Model, Files, Path, Out_Path, Num, Count1),
    !.
process_moon_circle(Model, [_ | Files], Path, Out_Path, Num, Count):-
    process_moon_circle(Model, Files, Path, Out_Path, Num, Count), !.

run_moon_circle(Num):-
    Path = '../../data/moon/',
    Out_Path = '../../out/moon/',
    atomic_concat(Path, 'train/', Train_Path),
    atomic_concat(Path, 'test/', Test_Path),
    atomic_concat(Out_Path, 'train/', Out_Train_Path),
    atomic_concat(Out_Path, 'test/', Out_Test_Path),
    directory_files(Train_Path, Train_Files),
    directory_files(Test_Path, Test_Files),
    load_model_svm('../../tmp/SVM_Moon.model', Model),
    %process_moon_circle(Model, Train_Files, Train_Path, Out_Train_Path, Num, 1),
    process_moon_circle(Model, Test_Files, Test_Path, Out_Test_Path, Num, 1),
    release_model_svm(Model).

light:-
    %run_protist(1),
    %run_protist(2),
    %run_protist(3),
    %run_protist(4),
    %run_protist(5),
    %run_protist(6),
    %run_protist(7),
    %run_protist(8),
    %run_protist(9),
    %run_protist(10),    
    %run_moon(1),
    %run_moon(2),
    %run_moon(3),
    %run_moon(4),
    %run_moon(5),
    %run_moon_circle(1),
    %run_moon_circle(2),
    %run_moon_circle(3),
    %run_moon_circle(4),
    %run_moon_circle(5),
    %run_moon_circle(6),
    %run_moon_circle(7),
    %run_moon_circle(8),
    %run_moon_circle(9),
    run_moon_circle(10).

