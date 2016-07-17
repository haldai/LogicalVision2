/* Active abduction module
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

/* Abduction of concepts from Observation*/
abduce(true):-
    !.
abduce(not(A)):-
    not(abduce(A)).
abduce((A, B)):-
    abduce(A),
    abduce(B).
abduce((A; B)):-
    abduce(A);
    abduce(B).
% do not parse built in clauses, call it
abduce(A):-
    (predicate_property(A, built_in);
     predicate_property(A, imported_from(_))),
    functor(A, Pred, _),
    not(Pred = (;); Pred = (,)),
    call(A).
% abducible predicates
% here the "abducible" is different with ALP definition
abduce(A):-
    not(predicate_property(A, built_in);
     predicate_property(A, imported_from(_))),    
    functor(A, Pred, Nargs),
    abducible(Pred/Nargs),
    call(A).
% samplable predicates
abduce(A):-
    not(predicate_property(A, built_in);
        predicate_property(A, imported_from(_))),    
    functor(A, Pred, Nargs),
    samplable(Pred/Nargs),
    call(A).
abduce(A):-
    not(predicate_property(A, built_in);
     predicate_property(A, imported_from(_))),
    clause(A, B),
    not(functor(B, call, _)),
    abduce(B).

%abduce(Target):-
%    Target =.. [Pred | Args],
%    abducible(Pred),
%    
%    fail.
%
%abduce_samp():-
%    !.


% TODO: What does Args look like? [[Args1], [Args2], ...]?


solve(true, true) :-!.
solve(not(A), not(ProofA)) :-
	not(solve(A, ProofA)).
solve((A, B), (ProofA, ProofB)) :-
	solve(A, ProofA), 
	solve(B, ProofB).
solve((A; B), (ProofA; ProofB)) :-
    solve(A, ProofA);
    solve(B, ProofB).
solve(A, (A :- ProofB)) :-
	clause(A, B),
    not(functor(B, call, _)),
	%not(B =.. [call | _]),
	solve(B, ProofB).

a(X):-
	b(X), c(X).
b(X):-
	(d(X); e(X)).
	
c(1).
c(2).
c(3).
d(1).
e(2).

p(X, Y) :- q(X), r(Y), z(X).
q(X) :- s(X).
r(X) :- t(X).
s(a).
z(a).
z(b).
t(b).
t(c).

%?- solve(a(X), B).
%@ X = 1,
%@ B =  (a(1):-(b(1):-(d(1):-true);_G1815), (c(1):-true)) ;
%@ X = 2,
%@ B =  (a(2):-(b(2):-_G1814;(e(2):-true)), (c(2):-true)) ;
%@ false.
%?- nodebug.
%?- abducible(xx), samplable(ff), abduce(a(X)).

