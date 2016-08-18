:-['../utils/utils.pl'].

num_arrays(a,
           [[1,2,3,4,5,6,7,8,9,10],
            [2,5,7,8,14,78,99],
            [2,3,5,7,4,9,10,12],
            [9,8,7,5,4,2,1],
            [29,1,85,109,23,82,91,0,-3],
            [2,4,5,3,5,7,8,4,3,2,1],
            [1,2,3,4,5,6,7,8,9,10,11,12,23],
            [5,6,7,4,3,5,7,8,9,3,2,2,4,6,7,8],
            [32,565,78,99,33,35,7,8,9,99,6,4],
            [54,3,2,6,7,8,3,3,4,5,6,7,8],
            [9,8,7,6,5,4,3,2,1],
            [99,88,77,66,55,44,43,33,22,11,1],
            [88,7,6,55,4,33,22,2],
            [9,0,7,6,5,4,5,5,6,6],
            [90,87,65,43,21,1]
           ]).

mono_arr(_, _, 0):-
    !.
mono_arr([], _, _):-
    !.
mono_arr(Arr, Ord, N):-
    Arr = [F | _],
    last(Arr, L),
    middle_element(Arr, M),
    (Ord = =<; Ord = >=),
    Cls1 =.. [Ord, F, M],
    Cls2 =.. [Ord, M, L],
    call(Cls1), call(Cls2),
    !,
    length(Arr, Len), Mid is round(Len / 2),
    interval(1, Mid, FHalf), interval(Mid, Len, LHalf),
    index_select(FHalf, Arr, Arr1),
    index_select(LHalf, Arr, Arr2),
    N1 is N - 1,
    mono_arr(Arr1, Ord, N1),
    mono_arr(Arr2, Ord, N1).

mono_arrays(_, [], 0):-
    !.
mono_arrays(Obs, [Arr | Arrs], N):-
    sample_array(Obs, Arr0),
    mono_arr(Arr0, Ord, 2),
    update_obs(Obs, Obs1, Arr0),
    Arr = Arr0,
    mono_arrays(Obs1, Arrs, N).
mono_arrays(Obs, Arrs, S_time):-
    % sample_array(Obs, Arr0),
    % not(mono_arr(Arr0, Ord, 2)),
    S_time1 is S_time - 1,
    mono_arrays(Obs, Arrs, S_time1).

sample_array(Obs, Arr):-
    length(Obs, Len),
    random(1, Len, N),
    nth1(N, Obs, Arr).

update_obs(Obs, Obs1, Arr):-
    select(Arr, Obs, Obs1), !.
