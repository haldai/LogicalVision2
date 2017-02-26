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
/* Active abduction module
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

:- dynamic
       abducible/1,
       samplable/1.

:- discontiguous
       abducible/1,
       samplable/1.

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

/*
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
%?- nodebug.
%?- assertz(abducible(xx)), assertz(samplable(ff)), abduce(a(X)).
%?- abduce(a(X)).
*/
