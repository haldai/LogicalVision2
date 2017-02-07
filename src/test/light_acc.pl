/* evaluation highlight calculation
 * =============================================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

:- discontiguous object/2.
:- discontiguous clock_angle/4.

:- ensure_loaded('../utils/utils.pl').

%:- ensure_loaded(['../test/run_light.pl']).

light_eval(Turn, Path, Error):-
    directory_files(Path, Files),
    light_error(Turn, Path, Files, Error, W1, W2, Total),
    write('Error 1: '), writeln(W1),
    write('Error 2: '), writeln(W2),
    Acc is 1 - (W1 + W2)/Total,
    write('Accuracy: '), writeln(Acc).

light_voted_eval(Path, Error):-
    directory_files(Path, Files),
    light_vote_error(Path, Files, Error, W1, W2, Total),
    write('Error 1: '), writeln(W1),
    write('Error 2: '), writeln(W2),
    Acc is 1 - (W1 + W2)/Total,
    write('Accuracy: '), writeln(Acc).

light_error(_, _, [], 0, 0, 0, 0):-
    !.
light_error(Turn, Path, [File | Files], Error, W1, W2, Total):-
    File \= '.', File \= '..',
    split_string(File, '.', '', [Name, "pl"]),
    atomic_concat(Path, File, File_Path),
    consult(File_Path),
    clock_angle(Turn, _, _, Predicted), % predicted
    string_codes(Name, [D1, D2 | _]), string_codes(Code, [D1, D2]),
    atom_number(Code, Label), % label
    clock_angle_err(Label, Predicted, Err),
    unload_file(File_Path),
    light_error(Turn, Path, Files, Error1, W11, W22, Total1),
    Total is Total1 + 1,
    Error is Error1 + Err,
    ((Err =:= 0, W1_ = 0, W2_ = 0);
     (Err =:= 1, W1_ = 1, W2_ = 0);
     (Err =:= 2, W1_ = 0, W2_ = 1)), !,
    W1 is W11 + W1_,
    W2 is W22 + W2_,
    !.
light_error(Turn, Path, [_ | Files], Error, W1, W2, Total):-
    % error = 0
    light_error(Turn, Path, Files, Error, W1, W2, Total), !.

light_vote_error(_, [], 0, 0, 0, 0):-
    !.
light_vote_error(Path, [File | Files], Error, W1, W2, Total):-
    File \= '.', File \= '..',
    split_string(File, '.', '', [Name, "pl"]),
    atomic_concat(Path, File, File_Path),
    consult(File_Path),
    findall(P, clock_angle(_, _, _, P), Preds),
    most_common_member(Preds, Predicted), % predicted
    string_codes(Name, [D1, D2 | _]), string_codes(Code, [D1, D2]),
    atom_number(Code, Label), % label
    clock_angle_err(Label, Predicted, Err),
    unload_file(File_Path),
    light_vote_error(Path, Files, Error1, W11, W22, Total1),
    Total is Total1 + 1,
    Error is Error1 + Err,
    ((Err =:= 0, W1_ = 0, W2_ = 0);
     (Err =:= 1, W1_ = 1, W2_ = 0, writeln(File));
     (Err =:= 2, W1_ = 0, W2_ = 1, writeln(File))), !,
    W1 is W11 + W1_,
    W2 is W22 + W2_,
    !.
light_vote_error(Path, [_ | Files], Error, W1, W2, Total):-
    % error = 0
    light_vote_error(Path, Files, Error, W1, W2, Total), !.


light_eval_protist_train(Turn, Error):-
    Path = '../../out/protist/train/',
    light_eval(Turn, Path, Error).
light_eval_protist_test(Turn, Error):-
    Path = '../../out/protist/test/',
    light_eval(Turn, Path, Error).
light_eval_moon_train(Turn, Error):-
    Path = '../../out/moon/train/',
    light_eval(Turn, Path, Error).
light_eval_moon_test(Turn, Error):-
    Path = '../../out/moon/test/',
    light_eval(Turn, Path, Error).

light_voted_eval_protist_train(Error):-
    Path = '../../out/protist/train/',
    light_voted_eval(Path, Error).
light_voted_eval_protist_test(Error):-
    Path = '../../out/protist/test/',
    light_voted_eval(Path, Error).
light_voted_eval_moon_train(Error):-
    Path = '../../out/moon/train/',
    light_voted_eval(Path, Error).
light_voted_eval_moon_test(Error):-
    Path = '../../out/moon/test/',
    light_voted_eval(Path, Error).

clock_angle_err(11, 12, 0).
clock_angle_err(12, 12, 0).
clock_angle_err(1, 12, 0).
clock_angle_err(2, 12, 1).
clock_angle_err(3, 12, 1).
clock_angle_err(4, 12, 1).
clock_angle_err(5, 12, 2).
clock_angle_err(6, 12, 2).
clock_angle_err(7, 12, 2).
clock_angle_err(8, 12, 1).
clock_angle_err(9, 12, 1).
clock_angle_err(10, 12, 1).

clock_angle_err(11, 3, 1).
clock_angle_err(12, 3, 1).
clock_angle_err(1, 3, 1).
clock_angle_err(2, 3, 0).
clock_angle_err(3, 3, 0).
clock_angle_err(4, 3, 0).
clock_angle_err(5, 3, 1).
clock_angle_err(6, 3, 1).
clock_angle_err(7, 3, 1).
clock_angle_err(8, 3, 2).
clock_angle_err(9, 3, 2).
clock_angle_err(10, 3, 2).

clock_angle_err(11, 6, 2).
clock_angle_err(12, 6, 2).
clock_angle_err(1, 6, 2).
clock_angle_err(2, 6, 1).
clock_angle_err(3, 6, 1).
clock_angle_err(4, 6, 1).
clock_angle_err(5, 6, 0).
clock_angle_err(6, 6, 0).
clock_angle_err(7, 6, 0).
clock_angle_err(8, 6, 1).
clock_angle_err(9, 6, 1).
clock_angle_err(10, 6, 1).

clock_angle_err(11, 9, 1).
clock_angle_err(12, 9, 1).
clock_angle_err(1, 9, 1).
clock_angle_err(2, 9, 2).
clock_angle_err(3, 9, 2).
clock_angle_err(4, 9, 2).
clock_angle_err(5, 9, 1).
clock_angle_err(6, 9, 1).
clock_angle_err(7, 9, 1).
clock_angle_err(8, 9, 0).
clock_angle_err(9, 9, 0).
clock_angle_err(10, 9, 0).

clock_angle_err(11, nil, 2).
clock_angle_err(12, nil, 2).
clock_angle_err(1, nil, 2).
clock_angle_err(2, nil, 2).
clock_angle_err(3, nil, 2).
clock_angle_err(4, nil, 2).
clock_angle_err(5, nil, 2).
clock_angle_err(6, nil, 2).
clock_angle_err(7, nil, 2).
clock_angle_err(8, nil, 2).
clock_angle_err(9, nil, 2).
clock_angle_err(10, nil, 2).
