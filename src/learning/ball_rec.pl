%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% learn ball recognition
%   ball grammar from a segment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module('metagol').

/***********************
Target Hypothesis:
=================



************************/

ballseg(Pic, Seg, Ball):-
    
%=======================
% Background Knowledge
%=======================
seg_size_limit(Seg, T):-
    findall(M, member([M, _], A), Xs),
    findall(M, member([_, M], A), Ys),
    min_list(Xs, Xmin), max_lisst(Xs, Xmax),
    min_list(Ys, Ymin), max_lisst(Ys, Ymax),
    sqrt((Xmax - Xmin)**2 + (Ymax - Ymin)**2) =< T.
