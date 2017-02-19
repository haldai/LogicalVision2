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
along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
************************************************************************/
 :- module(metagol,[learn/4,learn/3,pprint/1]).

:- user:call(op(950,fx,'@')).
:- user:use_module(library(lists)).

:- use_module(library(lists)).

:- dynamic(functional/0).
:- dynamic(min_clauses/1).
:- dynamic(max_clauses/1).
:- dynamic(metarule_next_id/1).
:- dynamic(background/1).

default(min_clauses(1)).
default(max_clauses(6)).
default(metarule_next_id(1)).

get_option(Option):-call(Option),!.
get_option(Option):-default(Option).

set_option(Option):-
  functor(Option,Name,Arity),
  functor(Retract,Name,Arity),
  retractall(Retract),
  assert(Option).

learn(_Name,Pos,Neg,G):- % deprecated
  write('WARNING: metagol learn(Name,...) is deprecated. Use learn/3 instead.'),nl,
  learn(Pos,Neg,G).

learn(Pos1,Neg1,G):-
  atom_to_list(Pos1,Pos2),
  atom_to_list(Neg1,Neg2),
  initial_sig(Pos2,Neg2,Sig1),!,
  proveall(Pos2,Sig1,Sig2,G),
  nproveall(Neg2,Sig2,G),
  (functional -> is_functional(Pos2,Sig2,G); true).

proveall(Atoms,Sig1,Sig2,G):-
  target_predicate(Atoms,Name/Arity),!,
  iterator(N,M),%% trace,
  format('% clauses: ~d invented predicates: ~d\n',[N,M]),
  augmented_sig(Name/Arity,M,Sig1,Sig2),
  prove(Atoms,Sig2,N,[],G).

prove([],_Sig,_MaxN,G,G).

prove([Atom|Atoms],Sig,MaxN,G1,G2):-
  prove_aux(Atom,Sig,MaxN,G1,G3),
  prove(Atoms,Sig,MaxN,G3,G2).

prove_aux('@'(Atom),_,_,G,G):- !,
  user:call(Atom).

prove_aux([P|Args],_,_,G,G):-
  user:primtest(P,Args),!,
  user:primcall(P,Args).

prove_aux(Atom,Sig,MaxN,G1,G2):-
  Atom = [P|Args],
  length(Args,A),
  %% trace,
  user:interpreted(P/A),!,
  user:background((Atom:-Body)),
  prove(Body,Sig,MaxN,G1,G2).

prove_aux([not,P|Args],_,_,G,G):-!,
  user:primtest(P,Args),
  \+ user:primcall(P,Args).

%% use existing abduction
prove_aux(Atom,Sig,MaxN,G1,G2):-
  %% need something to say not background knowledge
  Atom=[P|_Args],
  member(sub(Name,P,MetaSub),G1),
  user:metarule_init(Name,MetaSub,(Atom:-Body)),
  prove(Body,Sig,MaxN,G1,G2).

%% prove_aux(Atom,Sig,MaxN,G1,G2):-
%%   Atom = [P|_],
%%   member(P,[until,ifthenelse,map,fold,foldback,filter,reduce,reduceback]),!,
%%   user:background((Atom:-Body)),
%%   prove(Body,Sig,MaxN,G1,G2).

%% new abduction
prove_aux(Atom,Sig1,MaxN,G1,G2):-
  length(G1,L),
  L < MaxN,
  Atom=[P|_],
  (lower_sig(Atom,P,Sig1,Sig2) -> true; Sig2=Sig1),
  user:metarule(Name,MetaSub,(Atom :- Body),Sig2),
  new_metasub(G1,sub(Name,P,MetaSub)),
  prove(Body,Sig1,MaxN,[sub(Name,P,MetaSub)|G1],G2).

%% deterministic(X,_):- X = sub(pab_qac_pcb,_),!.
deterministic(sub(_,P,_),G):-
  \+ member(sub(_,P,_),G).

  %% X = sub(_,[H|_]),
  %% member(sub(Rule,[H|_]),G),
  %% Rule \= pab_qac_pcb,
  %% !, false.
%% deterministic(_,_).


new_metasub([],_).
new_metasub([H|_],A):-
  H == A,!,false.
new_metasub([_|T],A):-
  new_metasub(T,A).

prove_deduce(Atom,Sig,G):-
  length(G,N),
  prove_aux(Atom,Sig,N,G,G).

nproveall([],_Sig,_G).
nproveall([Atom|T],Sig,G):-
  \+ prove_deduce(Atom,Sig,G),
  nproveall(T,Sig,G).

iterator(N,M):-
  get_option(min_clauses(MinN)),
  get_option(max_clauses(MaxN)),!,
  between(MinN,MaxN,N),
  succ(MaxM,N),
  between(0,MaxM,M).

target_predicate([[P|Args]|_],P/A):-
  length(Args,A).

invented_symbols(0,_Name,[]):-!.
invented_symbols(M,Name,[InvSym/_Arity|Sig]) :-
  atomic_list_concat([Name,M],InvSym),
  succ(Prev,M),
  invented_symbols(Prev,Name,Sig).

lower_sig([P|Args],P,Sig1,Sig2):-
  length(Args,A),
  append(_,[P/A|Sig2],Sig1),!.

augmented_sig(P/A,M,Sig1,[P/A|Sig2]):-
  invented_symbols(M,P,InventedSymbols),
  append(InventedSymbols,Sig1,Sig2).

initial_sig(_Pos,_Neg,Prims):- !,
  findall(P/A,user:prim(P/A),Prims).

pprint(G1):-
  reverse(G1,G2),
  pprint_aux(G2).

pprint_aux([]).
pprint_aux([sub(Name,_P,MetaSub)|T]):-
  user:metarule_init(Name,MetaSub,Clause),
  copy_term(Clause,(ListHead:-ListBodyWithAts)),
  Head=..ListHead,
  convert_preds(ListBodyWithAts,AtomBodyList),
  (AtomBodyList==[] -> AtomClause=Head
                       ;
                       listtocomma(AtomBodyList,Body),
                       AtomClause=(Head:-Body)
  ),
  numbervars(AtomClause,0,_),
  format('~q.~n', [AtomClause]),
  pprint_aux(T).

listtocomma([],true):- !.
listtocomma([E],E):-!.
listtocomma([H|T],(H,R)):-
  listtocomma(T,R).

convert_preds([],[]).
convert_preds(['@'(Atom)|T],[Atom|R]):- !,
  convert_preds(T,R).
convert_preds([List|T],[Atom|R]):- !,
  Atom=..List,
  convert_preds(T,R).

atom_to_list([],[]).
atom_to_list([Atom|T],[AtomAsList|Out]):-
  Atom =..AtomAsList,
  atom_to_list(T,Out).

is_functional([],_,_).
is_functional([Atom|Atoms],Sig,G) :-
  user:func_test(Atom,Sig,G),
  is_functional(Atoms,Sig,G).

:- user:multifile(prim/1).
:- user:multifile(primcall/2).
:- user:multifile(primtest/2).

:- user:discontiguous(prim/1).
:- user:discontiguous(primcall/2).
:- user:discontiguous(primtest/2).

:- user:discontiguous(metarule/4).
:- user:discontiguous(metarule_init/3).
:- user:discontiguous(background/1).
%% :- dynamic().

gen_metarule_id(Id):-
  get_option(metarule_next_id(Id)),
  succ(Id,IdNext),
  set_option(metarule_next_id(IdNext)),!.

user:term_expansion(prim(P/A),[prim(P/A),primtest(P,Args),(primcall(P,Args):-Call)]):-
  functor(Call,P,A),
  Call=..[P|Args].

user:term_expansion(metarule(MetaSub,Clause),Asserts):-
  gen_body(MetaSub,Clause,PS,Body),
  user:term_expansion((metarule(MetaSub,Clause,PS):-Body),Asserts).

user:term_expansion((metarule(MetaSub,Clause,PS):-Body),Asserts):-
  is_list(MetaSub),
  gen_metarule_id(Name),
  user:term_expansion((metarule(Name,MetaSub,Clause,PS):-Body),Asserts).

user:term_expansion(metarule(Name,MetaSub,Clause),Asserts):-
  atom(Name),
  gen_body(MetaSub,Clause,PS,Body),
  user:term_expansion((metarule(Name,MetaSub,Clause,PS):-Body),Asserts).

user:term_expansion((metarule(Name,MetaSub,Clause,PS):-Body),Asserts):-
  add_metaruleinit((metarule(Name,MetaSub,Clause,PS):-Body),Asserts).

add_metaruleinit((metarule(Name,MetaSub,Clause,PS):-Body),Asserts):-
  Asserts = [
   (metarule(Name,MetaSub,Clause,PS):-Body),
   (metarule_init(Name,MetaSub,Clause))
  ].

%% automatically adds the bindings for the metasubs
gen_body(MetaSub,([P|_Args]:-Goals),PS,Body):-
  remove_var(P,MetaSub,MetaSubNew),
  gen_body_aux(MetaSubNew,Goals,PS,Body).

gen_body_aux([],_Goals,_PS,true):-!.

gen_body_aux(Vars,[[Var|Args]|Goals],PS,(member(Var/A,PS),Body)):-
  select_var(Var,Vars,NewVars),!,
  length(Args,A),
  gen_body_aux(NewVars,Goals,PS,Body).

gen_body_aux(Vars,[_|Goals],PS,Body):-
  gen_body_aux(Vars,Goals,PS,Body).

remove_var(_Var,[],[]).

remove_var(Var,[V|Vars],R):-
  Var==V,!,
  remove_var(Var,Vars,R).

remove_var(Var,[V|Vars],[V|R]):-
  remove_var(Var,Vars,R).

select_var(Var,[V|Vars],Vars):-
  Var==V,!.

select_var(Var,[V|Vars],[V|R]):-
  select_var(Var,Vars,R).