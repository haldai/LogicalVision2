%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logical Vision sampling predicates inplemented with Prolog
% ===========================================================
% Version: 2.0
% AUTHOR: WANG-ZHOU DAI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ['../scripts2/utils.pl'].

%================================
% 1d Gradients for sampled lines
%================================
% grad_l/a/b(+List_of_LAB, -Grad)
% given a set of LAB colors, return the gradient of brightness channel
grad_l(List_of_LAB, Grad):-
    grad_column(1, List_of_LAB, Grad).
grad_a(List_of_LAB, Grad):-
    grad_column(2, List_of_LAB, Grad).
grad_b(List_of_LAB, Grad):-
    grad_column(3, List_of_LAB, Grad).

% grad(+List, -Grad)
% compute gradient of a list of numbers, the first gradient is always 0
% e.g. grad([1, 2, 3, 5, 7], [0, 1, 1, 2, 2]).
grad(List, Grad):-
    grad(List, Grad, start).
grad([], [], _).
grad([L | Ls], [G | Gs], start):-
    G = 0,
    grad(Ls, Gs, L), !.
grad([L | Ls], [G | Gs], P):-
    G is L - P,
    grad(Ls, Gs, L).

% grad_column(+Column, +List_of_LAB, -Grad)
grad_column(Col, List_of_Vec, Grad):-
    column(Col, List_of_Vec, Column),
    grad(Column, Grad).
