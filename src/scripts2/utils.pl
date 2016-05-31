%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Learn statistical classifiers by abduction
%      AUTHOR: WANG-ZHOU DAI
% ========== Utility functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

% assert predicates 
asserta_pred([]).
asserta_pred([P | Ps]):- 
    asserta(pred(P)),
    asserta_pred(Ps).
% assert statistical classifiers
asserta_stat([]).
asserta_stat([P | Ps]):- 
    asserta(stat(P)),
    asserta_stat(Ps).

% difference between two vector, C = A - B
vec_diff([], [], []).
vec_diff([A | As], [B | Bs], [C | Cs]):-
    C is A - B,
    vec_diff(As, Bs, Cs).

% vector absolute value B = |A|
vec_abs([], []).
vec_abs([A | As], [B | Bs]):-
    B is abs(A),
    vec_abs(As, Bs).

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

%=================
% Object ordering
%=================
% Final Input and Output must be suffixes of initial Input and Output
obj_gt(atom_gt).
obj_gte(atom_gte).

atom_gt(X,Y,_):-
    X \== Y.   %X @< Y. 

atom_gte(_, _, _):-
	true. 

suffix(X, Y):-
    X == Y.			% Nonground suffix test
suffix(L, X):-
	nonvar(L), L=[_ | T],
	suffix(T, X).

%===========================
% Print predicates & clauses
%===========================
% print Program list to screen
printprogs([]).
printprogs([P | Ps]):-
    %write('DEBUG: printprogs/2'), nl,
    P = ps(Hyp, _, _, _),
    printprog(Hyp),
    printprogs(Ps).

% get Program as a list of metasubs
get_clause_meta_subs(Ps, Ms):-
    get_clause_meta_subs(Ps, Ms, []).

get_clause_meta_subs([], Ms, Ms).
get_clause_meta_subs([P | Ps], Ms, Temp):-
    P = ps(Hyp, _, _, _),
    append(Temp, Hyp, Temp1),
    get_clause_meta_subs(Ps, Ms, Temp1).

% print program to screen
printprog(MetaSub):-
    %write('DEBUG: printprog/1'), nl,
    copy_term(MetaSub, MetaSub1),
    reverse(MetaSub1, MetaSub2),
    converts(MetaSub2, Cs), nl, sort(Cs, Cs1),
    printclauses(Cs1), !.

converts([],[]):-
    !.
converts([metasub(RuleName, MetaSub) | MIs], [Clause | Cs]):-
    %write('DEBUG: converts/2'), nl,
    metarule(RuleName, MetaSub, Clause, _, _),
    numbervars(Clause, 0, _),
    converts(MIs, Cs), 
    !.

printclauses([]):-
    nl, !.
printclauses([C | Cs]):-
    %write('DEBUG: printclauses/1'), nl,
    printclause(C), nl,
    printclauses(Cs).

printclause((Head:-[])):-
    printatom(Head), write('.').
printclause((Head:-Body)) :-
    %write('DEBUG: printclause/1'), nl,
    printatom(Head), write(' :- '),
    printatoms(Body).

printatom_cond(List-_):-
    printatom(List).
printatom_cond(List):-
    %write('DEBUG: printatom_cond/1'), nl,
    printatom(List).

printatom(List):-
    %write('DEBUG: printatom/1'), nl,
    Atom =.. List, write(Atom).

printatoms([A]):-
    printatom_cond(A),
    write('.'), !.
printatoms([A | As]):-
    %write('DEBUG: printatoms/1'), nl,
    printatom_cond(A),
    write(', '),
    printatoms(As), !.

%============
% primitives
%============
prim(P):-
    primitives(P).

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

reverse(L1, L2):-
    reverse_(L1, [], L2).
reverse_([], L, L):-
    !.
reverse_([H | T], R, L):-
    reverse_(T, [H | R], L).

% Construct an ordered list of naturals in the interval [Lo,Hi]

interval(Lo, Hi, [Lo | T]) :-
    Lo =< Hi, Lo1 is Lo + 1,
    interval(Lo1, Hi, T), !.
interval(_, _, []).

%===================
% meta substitution
%===================
metasub(instance, Args):-
    Args = [P/_ | Args1],
    callatom([P | Args1]).

% predicates above: FOR PREDICATE VAR BINDING
% find the "Rest" and judge whether Q is in the "Rest"
pred_above(P, Q, Prog):-
        Prog = ps(_, sig(Ps, _), _, _),
        append(_, [P | Rest], Ps), element(Q, Rest).


%=====================================
% call an atom in [pred, args] format
%=====================================
callatom(Args):-
    Goal =.. Args,
    %write('CALLATOM PROVING '), write(Goal), nl,
    !, call(Goal).
  	%write('SUCCEEDED '), write(Goal), nl.

% my call atom all, find all possible bindings of var
callatom_binds(Args, Vars, New_binds):-
    not(ground(Args)),
    !,
    Goal =.. Args,
    %write('CALLATOM PROVING '), write(Goal), nl,
    !,
    findall(Vars, call(Goal), New_binds), !.
% call lists of pretests
callpre(Pres, FPre):-
    callseq(Pres, FPre).
% call a list of atoms and return which one has failed
callseq([], []).
callseq([A | As], R):-
    call(A) ->
        (callseq(As, R), !);
    (R = A, !).

%====================
% new predicate name
%====================
addnewpreds(_, N, N, Ps1, Ps1):-
    !.
addnewpreds(Name , N, M, Ps1, [P/_ | Ps2]) :-
    N1 is N + 1,
    newpred(Name, P, N1),
    N2 is N + 1,
    addnewpreds(Name, N2, M, Ps1, Ps2).

newpred(Name, P, N):-
    name(N, NC), % number to number characters
    append(Name, [95, 95 | NC], PC),
    name(P, PC), !.

newconst(X, N1, N2):-
    N is N1 + 48,
    name(X, [99, N]),
    N2 is N1 + 1, !.

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
index_select(Index_list, List, Return, Temp_list):-
    Index_list == [] ->
	(Return = Temp_list, !);
    (Index_list = [Idx | Tail],
     nth1(Idx, List, Ele),
     append(Temp_list, [Ele], Temp_list_),
     index_select(Tail, List, Return, Temp_list_)
    ).

index_select(Index_list, List, Return):-
    index_select(Index_list, List, Return, []).

% get the middle point of a list
middle_element([], []).
middle_element(List, Element):-
    length(List, Len),
    Idx is truncate(Len/2 + 0.5),
    nth1(Idx, List, Element).

% reverse a list
reverse([H | T], A, R):-
    reverse(T, [H | A], R). 
reverse([], A, A).

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
