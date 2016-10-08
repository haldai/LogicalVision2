/* Background knowledge for abducing lightsource
 */

% 1. sample point in image
% 2. radial sampling, get max directions
% 3. hypothesis: use max directions intersections,
% 4. validation: sample more point in image,
%     if (hypothized point - sampled point \in top 5% directions), then odd ++
%        else sample another point
% 5. validation success -> output,
%     else -> goto 3

:- ensure_loaded(['../utils/utils.pl']).
:- dynamic(evaled/1).

samplable(possible_dirs/3).
abducible(light_source/2).

% parameters
best_percentage(0.5).
eval_times(50).
step_size(30).
confidence_thresh(0.8).
same_dir_thresh(0.1745). % +-10 degrees

% for debug.
% size_2d(1, 640, 360).

%========================================
% Abducing light sources
%========================================

ab_light_source(_, _, 10, _, []):-
    !.
ab_light_source(Imgseq, Frame, T, Possible_Dirs, Sources):-
    T1 is T + 1,
    % samplable
    possible_dirs(Imgseq, Frame, Prp_dirs),
    % pairs_keys_values(Prp_dirs, _, Dirs),
    append(Prp_dirs, Possible_Dirs, New_prp_dirs),
    keysort(New_prp_dirs, New_Dirs_1),
    reverse(New_Dirs_1, New_Dirs_2),
    length(New_Dirs_2, L),
    (L =< 10 -> (L1 = L, !); (L1 = 10, !)),
    (prefix(New_Dirs, New_Dirs_2), length(New_Dirs, L1), !),
    pairs_keys_values(New_Dirs, _, Dirs),
    % @TODO: findall??
    % abducible
    ((light_source(Dirs, Src),
      % evaluation
      eval_light_source(Imgseq, Frame, Src, Prob)) ->
         (append([Prob-Src], Sources, Sources1), !);
     (Sources1 = Sources, !)),
    ab_light_source(Imgseq, Frame, T1, New_Dirs, Sources1).

%========================================
% Samplables
%========================================

% possible_dirs(+Imgseq, +Frame, -Prp_dirs)
% *SAMPLE* possible directions
% get max grad+/grad- ratio directions of a point
% returned Prp_dirs is a (key-item) map of (Ratio-[Point, Dir])
possible_dirs(Imgseq, Frame, Prp_dirs):-
    point_in_img(Imgseq, Frame, [X, Y, Frame]), % randomly sample a point
    step_size(Step),
    best_dirs(Imgseq, [X, Y, Frame], Step, Prp_dirs).

%========================================
% Abducibles
%========================================

% light_source(+Dirs, -Source):-
% Abduce a light source position
% Dirs is a list of directions, each direction vector is [Start, Dir],
%   where Start is the coordinate of starting point, Dir is the direction
light_source(Dirs, Source):-
    select(D1, Dirs, Dirs1),
    member(D2, Dirs1),
    D1 = [P, _], D2 = [Q, _], P \= Q,
    ray_source_intsct(D1, D2, Source),
    not(evaled(Source)),
    write(D1), write("\t"), write(D2),
    write("\t\t\t\t\tSource:\t"), write(Source).

%========================================
% Evaluations
%========================================

% eval_light_source(+Imgseq, +Frame, +Source, -Prob)
% evaluate abduced light source position and return the probability
eval_light_source(Imgseq, Frame, Source, Prob):-
    eval_times(T),
    eval_light_source(Imgseq, Frame, Source, Prob, T, 0, 0), !,
    assertz(evaled(Source)),
    write("\t->"), write(Prob), nl,
    confidence_thresh(Conf),
    Prob > Conf.

eval_light_source(_, _, _, Prob, 0, Success, Fail):-
    % probability of the Source to be ground truth
    Prob is Success*1.0/(Success + Fail), !.
eval_light_source(Imgseq, Frame, Source, Prob, T, Success, Fail):-
    point_in_img(Imgseq, Frame, Point), % sample a point
    % get best guesses of light ray direction
    step_size(Step), best_dirs(Imgseq, Point, Step, Prp_dirs),
    pairs_keys_values(Prp_dirs, _, Dirs),
    % compare Source2Point direction with best light rays
    Source = [X1, Y1, Frame], Point = [X2, Y2, Frame],
    Test_dir = [[X1, Y1, Frame], [X2, Y2, 0]],
    dir_close_to_one_of_dirs(Test_dir, Dirs),
    % if success
    T1 is T - 1, Success1 is Success + 1,
    eval_light_source(Imgseq, Frame, Source, Prob, T1, Success1, Fail).
eval_light_source(Imgseq, Frame, Source, Prob, T, Success, Fail):-
    % if failed
    T1 is T - 1, Fail1 is Fail + 1,
    eval_light_source(Imgseq, Frame, Source, Prob, T1, Success, Fail1).

%========================================
% Primitives
%========================================

% best_dirs(+Imgseq, +Position, +Step, -Prp_dirs)
% best directions in position [X, Y, Frame] of Imgseq
best_dirs(Imgseq, [X, Y, Frame], Step, Prp_dirs):-
    radial_lines_2d([X, Y, Frame], 0, 359, Step, Rays), % sample radial lines
    sample_lines_L_grads(Imgseq, Rays, Pts, Gs),
    % calculate grad+/grad- proportions
    grad_prop(Pts, Gs, Prps),
    % make pair: (Proportion-Ray)
    pairs_keys_values(Prp_ray, Prps, Rays),
    % sort and get decreasing ordered ray list
    keysort(Prp_ray, Prp_ray_sorted1),
    reverse(Prp_ray_sorted1, Prp_ray_sorted),
    % number of best directions
    length(Prp_ray_sorted, N), best_percentage(T), M is ceil(N*T),
    % return best directions
    (prefix(Prp_dirs, Prp_ray_sorted), length(Prp_dirs, M), !).

% point_in_img(+Imgseq, +Frame, ?[X, Y]).
% sample a point in image (frame in image sequence)
point_in_img(Imgseq, Frame, [X, Y, Frame]):-
    var(X), var(Y),
    size_2d(Imgseq, W, H),
    random(0, W, X), random(0, H, Y),
    !. % random position

/* proportion of gradients (Pos/Neg) */
grad_prop([], [], []):-
    !.
grad_prop([Pts | Points], [Grd | Grads], [P | Props]):-
    items_key_geq_T(Pts, Grd, 2, Pos), % grad+ (>=2)
    items_key_less_T(Pts, Grd, -1, Neg), % grad- (< -1)
    length(Pos, NPos), length(Neg, NNeg),
    NPos + NNeg > 20, % no trivial directions (few brightness changed points)
    P is NPos/(NNeg + NPos + 10e-10),
    grad_prop(Points, Grads, Props).
grad_prop([_ | Points], [_ | Grads], [P | Props]):-
    P = -1,
    grad_prop(Points, Grads, Props).

/* direction Test is close to one of direction in Dirs */
dir_close_to_one_of_dirs(_, []):-
    fail, !.
dir_close_to_one_of_dirs(Test, [D | _]):-
    Test = [[X1, Y1, F], [X2, Y2, 0]], D = [[X2, Y2, F], [X3, Y3, 0]],
    angle([X1, Y1], [X2, Y2], [X3, Y3], Ang),
    same_dir_thresh(Th), abs(Ang - pi) =< Th, !.
dir_close_to_one_of_dirs(Test, [_ | Dirs]):-
    dir_close_to_one_of_dirs(Test, Dirs), !.

% angle of three points "AB and BC"
angle([X1, Y1], [X2, Y2], [X3, Y3], A):-
    dot([X2 - X1, Y2 - Y1], [X2 - X3, Y2 - Y3], P),
    eu_dist([X2 - X1, Y2 - Y1], [0, 0], D1),
    eu_dist([X2 - X3, Y2 - Y3], [0, 0], D2),
    Cos is round(P/(D1*D2 + 10e-10)*10000)/10000, % 4 digits after '.'
    A is acos(Cos).

%
ray_source_intsct(A, B, Source):-
    A = [[X2, Y2, F], [X1, Y1, 0]],
    B = [[X4, Y4, F], [X3, Y3, 0]],
    % inverse the direction of rays
    % RayA = [[X1, Y1], [X2 - X1, Y2 - Y1]],
    % RayB = [[X3, Y3], [X4 - X3, Y4 - Y3]],
    SAX = X1, SAY = Y1, DAX is X2 - X1, DAY is Y2 - Y1,
    SBX = X3, SBY = Y3, DBX is X4 - X3, DBY is Y4 - Y3,
    DX is SBX - SAX,
    DY is SBY - SAY,
    Det is DBX * DAY - DBY * DAX,
    Det \== 0, % if Det == 0, no intersection
    U is (DY * DBX - DX * DBY) * 1.0 / Det,
    V is (DY * DAX - DX * DAY) * 1.0 / Det,
    U > 1, V > 1, % have intersection before two rays start
    SX is round(SAX + DAX * U),
    SY is round(SAY + DAY * U),
    Source = [SX, SY, F].


%?- gtrace.
%@ % The graphical front-end will be used for subsequent tracing
%@ true.
%@ % The graphical front-end will be used for subsequent tracing
%@ true.
%?- ray_source_intsct([[479,205,0],[5000000,8660250,0]], [[162,199,0],[5000000,8660254,0]], D).
%?- ray_source_intsct([[7,5,0],[8660254,5000000,0]], [[211,47,0],[5000000,8660254,0]], D).
%@ % Execution Aborted

