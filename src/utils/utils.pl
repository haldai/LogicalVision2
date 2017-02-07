%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% My Prolog utility functions
% ===========================
% AUTHOR: WANG-ZHOU DAI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded('../utils/geometry.pl').

% arithmetic functions
product(V1, V2, V3):-
    V3 is V1 * V2.
sum(V1, V2, V3):-
    V3 is V1 + V2.
minus(V1, V2, V3):-
    V3 is V1 - V2.
divide(0, 0, 0):-
    !.
divide(_, 0, _):-
    fail, !.
divide(V1, V2, V3):-
    V3 is V1 / V2, !.
divide0(_, 0, 0):- % restrict x/0 = 0
    !.
divide0(X, Y, Z):-
    divide(X, Y, Z).
predecessor(N, M):-
    M is N - 1.
successor(N, M):-
    M is N + 1.
% for labeling
greater_than_zero(A, 1):-
    A > 0, !.
greater_than_zero(A, 0):-
    A =< 0, !.

less_than(A, B):-
    A < B.
greater_than(A, B):-
    A > B.
leq(A, B):-
    A =< B.
geq(A, B):-
    A >= B.

% generate temporary variables
temp_vars(0, Return, Temp):-
    Return = Temp, !.
temp_vars(N, Return, Temp):-
    N1 is N - 1,
    append(Temp, [_], Temp_1),
    temp_vars(N1, Return, Temp_1).
temp_vars(N, List):-
    integer(N),
    N >= 0,
    temp_vars(N, List, []).

% find the rest part of the list
append([], L, L).
append([H | T], L, [H | R]):-
    append(T, L, R), !.

% append list of lists into a list
append_lists([], []).
append_lists([L | Ls], As):-
	append(L, Ws, As),
	append_lists(Ls, Ws).

group_pairs_by_numbers(_, -1, []):-
    !.
group_pairs_by_numbers(Pairs, N, [N-G | Groups]):-
    findall(V, member(N-V, Pairs), G), !,
    N1 is N - 1,
    group_pairs_by_numbers(Pairs, N1, Groups).

% difference between two vector, C = A - B
vec_diff(A, B, C):-
    maplist(minus, A, B, C).

% B - A
vec_neg_diff(A, B, C):-
    maplist(minus, B, A, C).

vec_sum(A, B, C):-
    maplist(sum, A, B, C).

vec_neg(A, B):-
    maplist(product(-1), A, B).    

vec_multiply(V, A, B):-
    maplist(product(V), A, B).

vec_divide(_, 0, []):-
    !.
vec_divide(A, V, B):-
    length(A, L),
    init_vec(L, V, V1),
    maplist(divide, A, V1, B).

% vector absolute value B = |A|
vec_abs([], []).
vec_abs([A | As], [B | Bs]):-
    B is abs(A),
    vec_abs(As, Bs).

% sum of vector list
sum_vec_list([], 0):-
    !.
sum_vec_list(Vec, Sum):-
    Vec = [V | _],
    length(V, L),
    zeros(L, Temp),
    sum_vec_list(Vec, Temp, Sum).
sum_vec_list([], Sum, Sum):-
    !.
sum_vec_list([V | Vs], Temp, Sum):-
    vec_sum(V, Temp, Temp1),
    sum_vec_list(Vs, Temp1, Sum).

% initialize a list of zeros
init_vec(0, _, []):-
    !.
init_vec(Length, Value, [Value | Vecs]):-
    L is Length - 1,
    init_vec(L, Value, Vecs).

zeros(0, []):-
    !.
zeros(N, [0 | V]):-
    N1 is N - 1,
    zeros(N1, V).

% vector indices which exceeds a threshold
vec_thresh_idx(V, T, R):-
    length(V, L),
    L > 0,
    findall(N, between(1, L, N), Is),
    vec_thresh_idx(V, Is, T, R).
vec_thresh_idx([], [], _, []).
vec_thresh_idx([V | Vs], [I | Is], T, [R | Rs]):-
    V >= T,
    R = I,
    vec_thresh_idx(Vs, Is, T, Rs), !.
vec_thresh_idx([V | Vs], [_ | Is], T, [R | Rs]):-
    V < T,
    vec_thresh_idx(Vs, Is, T, [R | Rs]), !.

% average of list
average([], 0):-
    !.
average(A, B):-
    A = [E | _], number(E), % list of numbers
    sum_list(A, S),
    length(A, L),
    B is S/L,
    !.
% average of vectors
average(A, B):-
    A = [E | _], is_list(E), % list of vectors
    sum_vec_list(A, Sum),
    length(A, L),
    vec_divide(Sum, L, B),
    !.

% 2 norm of an vector
norm_2(Vec, N):-
    norm_2_sum(Vec, Sum),
    N is sqrt(Sum).
norm_2_sum([], 0):-
    !.
norm_2_sum([X | Xs], Sum):-
    norm_2_sum(Xs, Sum1),
    Sum is Sum1 + X**2.

% mode of list
% Daniel Lyons@http://stackoverflow.com/questions/14691479/how-to-find-the-mode-of-a-list-in-prolog
count_of([], _,  0).
count_of([H|Rest], E, N1):- 
  equality_reified(H, E, Bool),
  count_of(Rest, E, N0),
  (Bool == true -> N1 is N0 + 1 ; N1 = N0).

frequency(L, I, N):-
  sort(L, LSorted),
  member(I, LSorted),
  count_of(L, I, N).

equality_reified(X, Y, R) :- X == Y, !, R = true.
equality_reified(X, Y, R) :- ?=(X, Y), !, R = false. % syntactically different
equality_reified(X, Y, R) :- X \= Y, !, R = false. % semantically different
equality_reified(X, X, true).
equality_reified(X, Y, false) :-
   dif(X, Y).

mode(L, X):-
  frequency(L, X, NMax),
  \+ (frequency(L, _, NBigger),
      NMax < NBigger).

%===========================
% difference/gradient
%===========================
% grad(+List, -Grad)
% compute gradient of a list of numbers, the first gradient is always 0
% e.g. grad([1, 2, 3, 5, 7], [0, 1, 1, 2, 2]).
grad(List, Grad):-
    grad(List, Grad, start).
grad([], [], _):-
    !.
grad([L | Ls], [0 | Gs], start):-
    grad(Ls, Gs, L), !.
grad([L | Ls], [G | Gs], P):-
    G is L - P,
    grad(Ls, Gs, L), !.

% grad_column(+Column, +List_of_LAB, -Grad)
grad_column(Col, List_of_Vec, Grad):-
    column(Col, List_of_Vec, Column),
    grad(Column, Grad).

% gradient of histograms
grad_KL(Hists, Grad):-
    grad_KL(Hists, Grad, start).
grad_KL([], [], _):-
    !.
grad_KL([L | Ls], [0 | Gs], start):-
    grad_KL(Ls, Gs, L), !.
grad_KL([L | Ls], [G | Gs], P):-
    compare_hist(P, L, G),
    grad_KL(Ls, Gs, L), !.

%=============================================
% zero item: return the items that key is 0.0
%=============================================
zero_item(L, Re):-
    extrema_thresh(T), zero_item(L, T, Re).

zero_item([_, _], _, []):-!.
zero_item([K1-I, K2-I2 | Pairs], T, [I | Is]):-
    K1*K2 < T,
    zero_item([K2-I2 | Pairs], T, Is).
zero_item([K1-_, Z1-I, K2-I2 | Pairs], T, [I | Is]):-
    K1*K2 < T,
    Z1 =:= 0,
    zero_item([K2-I2 | Pairs], T, Is), !.
zero_item([K1-_, Z1-I, Z2-_, K2-I2 | Pairs], T, [I | Is]):-
    K1*K2 < T,
    Z1 =:= 0, Z2 =:= 0,
    zero_item([K2-I2 | Pairs], T, Is), !.
zero_item([K1-_, Z1-_, Z2-I, Z3-_, K2-I2 | Pairs], T, [I | Is]):-
    K1*K2 < T,
    Z1 =:= 0, Z2 =:= 0, Z3 =:= 0,
    zero_item([K2-I2 | Pairs], T, Is), !.
zero_item([K1-_, Z1-_, Z2-I, Z3-_, Z4-_, K2-I2 | Pairs], T, [I | Is]):-
    K1*K2 < T,
    Z1 =:= 0, Z2 =:= 0, Z3 =:= 0, Z4 =:= 0,
    zero_item([K2-I2 | Pairs], T, Is), !.
zero_item([K1-_, Z1-_, Z2-_, Z3-I, Z4-_, Z5-_, K2-I2 | Pairs], T, [I | Is]):-
    K1*K2 < T,
    Z1 =:= 0, Z2 =:= 0, Z3 =:= 0, Z4 =:= 0, Z5 =:= 0,
    zero_item([K2-I2 | Pairs], T, Is), !.
zero_item([_-_, K-I | Pairs], T, Is):-
    zero_item([K-I | Pairs], T, Is), !.

%=================================================================
% get items whose keys are greater/less than threshold,
%     i.e. the map of points and gradients (G-[X, Y, Z]).
%=================================================================
items_key_geq_T(Items, Keys, Thresh, Return):-
    idx_element_geq_T(Keys, Thresh, Indices),
    index_select(Indices, Items, Return).

items_key_less_T(Items, Keys, Thresh, Return):-
    idx_element_less_T(Keys, Thresh, Indices),
    index_select(Indices, Items, Return).

% element_idx_geq_T(+List, +Thresh, -Indices)
idx_element_geq_T(List, Thresh, Indices):-
    idx_element_geq_T(List, Thresh, Indices, 1).

% index start from 1
idx_element_geq_T([], _, [], _):-
    !.
idx_element_geq_T([E | Es], Thresh, [N | Ns], N):-
    E >= Thresh,
    N1 is N + 1,
    idx_element_geq_T(Es, Thresh, Ns, N1), !.
idx_element_geq_T([E | Es], Thresh, Ns, N):-
    E < Thresh,
    N1 is N + 1,
    idx_element_geq_T(Es, Thresh, Ns, N1), !.

% element_idx_geq_T(+List, +Thresh, -Indices)
idx_element_less_T(List, Thresh, Indices):-
    idx_element_less_T(List, Thresh, Indices, 1), !.

% index start from 1
idx_element_less_T([], _, [], _):-
    !.
idx_element_less_T([E | Es], Thresh, [N | Ns], N):-
    E < Thresh,
    N1 is N + 1,
    idx_element_less_T(Es, Thresh, Ns, N1), !.
idx_element_less_T([E | Es], Thresh, Ns, N):-
    E >= Thresh,
    N1 is N + 1,
    idx_element_less_T(Es, Thresh, Ns, N1), !.

%=================================================================
% sort a mapping
%     i.e. the map of points and gradients (G-[X, Y, Z]).
%=================================================================
mapsort(Keys, Values, S_Keys, S_Values):-
    pairs_keys_values(Pairs, Keys, Values),
    keysort(Pairs, S_Pairs),
    pairs_keys_values(S_Pairs, S_Keys, S_Values).

%========================================
% count the most repeated item in list
%  http://stackoverflow.com/a/13674376
%========================================
most_common_member(L, M):-
    setof(I-E, C^(aggregate(count, member(E, L), C), I is -C), [_-M|_]).

%=================
% switch control
%=================
switch(X, [Val:Goal | Cases]):-
    (X = Val ->
         (call(Goal), !);
     switch(X, Cases)
    ).

%==========================
% Peano number operations
%==========================
peano(0, 0):-
    !.
peano(N, s(M)):-
    not(var(N)), !,
    N1 is N-1,
    peano(N1, M), !.
peano(N, s(M)):-
    var(N), !,
    peano(N1, M),
    N is N1+1, !.

% Construct an ordered list of naturals in the interval [Lo,Hi]
interval(Lo, Hi, [Lo | T]) :-
    Lo =< Hi, Lo1 is Lo + 1,
    interval(Lo1, Hi, T), !.
interval(_, _, []).

%================
% list operators
%================

% list members
element(H, [H | _]).
element(H, [_ | T]):-
    element(H,T).

% write list
print_list_ln(L):-
    forall(element(X, L),
	   writeln(X)
	  ).
print_list(L):-
    L == [] ->
	(writeln(""), !);
    (write("["),
     L = [H | T],
     write(H),
     forall(element(X, T),
	    (write(", "),
	     write(X)
	    )
	   ),
     writeln("]")
    ).

% print list without new line
print_list_noln(L):-
    L == [] ->
	(write(""), !);
    (write("["),
     L = [H | T],
     write(H),
     forall(element(X, T),
	    (write(", "),
	     write(X)
	    )
	   ),
     write("]")
    ).

% list delete
list_delete([], _, []).
list_delete(List, [], List).
list_delete(List, Del_list, Out_list):-
    Del_list = [Head | Tail],
    delete(List, Head, List_2),
    list_delete(List_2, Tail, Out_list),
    !.

% list add without duplication
list_add_nodup(L, [], L):-
    !.
list_add_nodup(List, Add, Return):-
    Add = [Head | Tail],
    (element(Head, List) ->
	     (list_add_nodup(List, Tail, Return), !);
     (append(List, [Head], List_2),
      list_add_nodup(List_2, Tail, Return),
      !)
    ), !.

% list insertion
% from http://stackoverflow.com/questions/10063516/list-length-inserting-element
list_insert(Val, [H | List], Pos, [H | Res]):- 
    Pos > 1, !, 
    Pos1 is Pos - 1, list_insert(Val, List, Pos1, Res). 
list_insert(Val, List, 1, [Val | List]).

% elements in list 1 that are not member of list 2
list_complement(List_1, List_2, Return, Temp):-
    List_1 == [] ->
	(Return = Temp, !);
    (List_1 = [Head | Tail],
     (\+element(Head, List_2) ->
	  (append(Temp, [Head], Temp_1),
	   list_complement(Tail, List_2, Return, Temp_1),
	   !
	  );
      list_complement(Tail, List_2, Return, Temp)
     ),
     !
    ).
list_complement(List_1, List_2, Return):-
    list_complement(List_1, List_2, Return, []).

% get a list of set of elements according to a list of indices
index_select(X, Y, Z):-
    index_select1(X, Y, Z).
index_select1([], _, []):-
    !.
index_select1([I | Idcs], List, [E | Elements]):-
    nth1(I, List, E),
    index_select1(Idcs, List, Elements), !.

index_select0([], _, []):-
    !.
index_select0([I | Idcs], List, [E | Elements]):-
    nth0(I, List, E),
    index_select0(Idcs, List, Elements), !.

mask_select(_, [], []):-
    !.
mask_select([], _, []):-
    !.
mask_select([1 | Ms], [E | Es], [E | Re]):-
    mask_select(Ms, Es, Re), !.
mask_select([0 | Ms], [_ | Es], Re):-
    mask_select(Ms, Es, Re), !.

% random select an item in list
random_select([], []).
random_select(List, Item):-
    length(List, Len),
    random(0, Len, Idx),
    nth0(Idx, List, Item).

% find the index of max number in list of numbers
max_list_idx(List, Re):-
    max_list_idx(List, 1, Re, 1), !.
max_list_idx(List, Crt, Temp, Temp):-
    length(List, Len),
    Crt > Len, !.
max_list_idx(List, Crt, Re, Temp):-
    nth1(Crt, List, E_c),
    nth1(Temp, List, E_t),
    E_c >= E_t,
    Crt1 is Crt + 1,
    max_list_idx(List, Crt1, Re, Crt), !.
max_list_idx(List, Crt, Re, Temp):-
    Crt1 is Crt + 1,
    max_list_idx(List, Crt1, Re, Temp).

% column(Idx, Vectors, Col)
% get a column from a list of vector
column(_, [], []):-
    !.
column(Idx, [V | Vs], [C | Cs]):-
    nth1(Idx, V, C),
    column(Idx, Vs, Cs).

% get the middle point of a list
middle_element([], []).
middle_element(List, Element):-
    length(List, Len),
    Idx is round(Len/2),
    nth1(Idx, List, Element).

/*% reverse a list
reverse([H | T], A, R):-
    reverse(T, [H | A], R). 
reverse([], A, A).
*/

% reverse a list
reverse(L1, L2):-
    reverse_(L1, [], L2).
reverse_([], L, L):-
    !.
reverse_([H | T], R, L):-
    reverse_(T, [H | R], L).

% indices combinations: comb_idx/3.
% unordered combinations for natural number (N >= 0)
comb_idx(0, _, []).
comb_idx(N, [X | T], [X | Comb]):-
    N > 0,
    N1 is N - 1,
    comb_idx(N1, T, Comb).

comb_idx(N, [_ | T], Comb):-
    N > 0,
    comb_idx(N, T, Comb).

% combinations of elements in list: combination/3
% combinations of N members in List
combination(N, List, Combs):-
    length(List, Len),
    findall(Num, between(1, Len, Num), Idx_list),
    findall(L, comb_idx(N, Idx_list, L), Comb_idx_list),
    findall(Comb_element, 
	    (element(Comb_idx, Comb_idx_list),
	     index_select(Comb_idx, List, Comb_element)),
	    Combs).

% cartesian product of two lists
cartesian_product([], _, []):-
    !.
cartesian_product([A | As], B, Re):-
    cartesian_product(As, B, Re1),
    set_product(A, B, Re2),
    append(Re2, Re1, Re),
    !.
set_product(_, [], []):-
    !.
set_product(A, [B | Bs], [[A, B] | Re]):-
    set_product(A, Bs, Re),
    !.
    
% index of element in list
indexof(Index, Item, List):-
    nth1(Index, List, Item).
indexof(-1, _, _).

% take first/last n elements from a list
firstN(N, _, Xs):-
    N =< 0, !,
    N =:= 0,
    Xs = [], !.
firstN(_, [], []):-
    !.
firstN(N, [X | Xs], [X | Ys]):-
    M is N - 1,
    firstN(M, Xs, Ys).

lastN(N, L, R):-
    length(L, S),
    N1 is S - N,
    lastT(N1, L, R).
lastT(0, L, L):- !.
lastT(N, [_ | T], L):-
    N1 is N - 1,
    lastT(N1, T, L).

%=============
% my find all
%=============
my_findall(X, Goal, Xlist):-
    call(Goal),
    assertz(my_queue(X)),
    fail;
    assertz(my_queue(bottom)),
    my_collect(Xlist).
my_collect(L):-
    retract(my_queue(X)), !,
    (X == bottom, !, L = []
     ;
     L = [X | Rest], my_collect(Rest)).


%==========================================================================
% varnumbers is a partial inverse to numbervars/3
% Modefied from Richard A. O'Keefe's code by daiwz
% http://swi-prolog.996271.n3.nabble.com/SWIPL-Undo-numbervars-3-td210.html
%==========================================================================
varnumbers(T0, T) :-
    varnumbers(T0, T, '$VAR', 0).

varnumbers(T0, T, N0) :-
    varnumbers(T0, T, '$VAR', N0).

varnumbers(T0, T, F, N0) :-
    integer(N0),
    N1 is N0 - 1,
    max_var_number_(T0, F, N1, N),
    Number_Of_Variables is N - N1,
    functor(Vars, '$VAR', Number_Of_Variables),
    varnumbers_(T0, T, F, N1, Vars).

max_var_number_(T0, F, N1, N) :-
    (   var(T0) -> N = N1
	;   functor(T0, Symbol, Arity),
            (   Arity < 1 -> N = N1
		;   Arity = 1 ->
			arg(1, T0, A0),
			(   Symbol == F, integer(A0) ->
				(   A0 > N1 -> N = A0
				    ;/* A0=< N1 */ N = N1
				)
			    ;/* not a $VAR(N) term */
			    max_var_number_(A0, F, N1, N)
			)
		;   max_var_number_(T0, F, N1, N, Arity)
            )
    ).

max_var_number_(T0, F, N1, N, I) :-
    arg(I, T0, A0),
    (   I > 1 ->
            max_var_number_(A0, F, N1, N2),
            J is I - 1,
            max_var_number_(T0, F, N2, N, J)
	;   max_var_number_(A0, F, N1, N)
    ).

varnumbers_(T0, T, F, N1, Vars) :-
    (   var(T0) -> T = T0
	;   functor(T0, Symbol, Arity),
            (   Arity < 1 -> T = T0
		;   (Arity = 1, Symbol = F) ->
			arg(1, T0, A0),
			(  integer(A0), A0 > N1 ->
				I is A0 - N1,
				arg(I, Vars, T)
			    ;   arg(1, T0, AAAA),
				varnumbers_(A0, AAAA, F, N1, Vars)
			)
		;   functor(T, Symbol, Arity),
		    varnumbers_(T0, T, F, N1, Vars, Arity)
            )
    ).

varnumbers_(T0, T, F, N1, Vars, I) :-
    arg(I, T0, A0),
    arg(I, T,  AAAA),
    (   I > 1 ->
            varnumbers_(A0, AAAA, F, N1, Vars),
            J is I - 1,
            varnumbers_(T0, T, F, N1, Vars, J)
	;   varnumbers_(A0, AAAA, F, N1, Vars)
    ).

