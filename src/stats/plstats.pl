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
/* Statistical models module
 * ============================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */
:- load_foreign_library(foreign('../../libs/cvstats.so')),
   load_foreign_library(foreign('../../libs/cvcluster.so')).

/*
 * Labeled data are represented by a list of key-value maps
 * i.e., [X1-Y1, X2-Y2, ...], where Xi = [Xi1, Xi2, ...] is the feature vector,
 * Yi = 0 or 1 is the label (CURRENTLY ONLY SINGLE LABEL PROBLEM IS CONSIDERED).
 */

%%%%%%%%%%%%%%%%%% evaluate binary labels %%%%%%%%%%%%%%%%%%%
eval_bin_acc(Labels, Predicted, Acc):-
    length(Labels, N),
    eval_bin_labels(Labels, Predicted, TP, _, TN, _),
    Acc is (TP + TN)/N.
eval_bin_precision(Labels, Predicted, Precision):-
    eval_bin_labels(Labels, Predicted, TP, FP, _, _),
    Sum is TP + FP, divide(TP, Sum, Precision).
eval_bin_recall(Labels, Predicted, Recall):-
    eval_bin_labels(Labels, Predicted, TP, _, _, FN),
    Sum is TP + FN, divide(TP, Sum, Recall).

eval_bin_labels([], [], 0, 0, 0, 0):-
    !.
eval_bin_labels([1 | Ls], [1 | Ps], TP, FP, TN, FN):-
    eval_bin_labels(Ls, Ps, TP1, FP, TN, FN),
    TP is TP1 + 1, !.
eval_bin_labels([0 | Ls], [1 | Ps], TP, FP, TN, FN):-
    eval_bin_labels(Ls, Ps, TP, FP1, TN, FN),
    FP is FP1 + 1, !.
eval_bin_labels([0 | Ls], [0 | Ps], TP, FP, TN, FN):-
    eval_bin_labels(Ls, Ps, TP, FP, TN1, FN),
    TN is TN1 + 1, !.
eval_bin_labels([1 | Ls], [0 | Ps], TP, FP, TN, FN):-
    eval_bin_labels(Ls, Ps, TP, FP, TN, FN1),
    FN is FN1 + 1, !.
    
%%%%%%%%%%%%%% AdaBoost %%%%%%%%%%%%%%%%

% neglect some parameters
train_adaboost(Data_label, Model):-
    write("Training AdaBoost model: "),
    time(train_adaboost(Data_label, 2, 200, 500, 1e-10, Model)),
    writeln("Training AdaBoost complete!").

%%%%%%%%%%%%% SVM %%%%%%%%%%%%%%
train_svm(Data_Label, Model):-
    write("Training SVM: "),
    time(train_svm(Data_Label,
                   '-g 0.0039 -c 100000 -h 0',
                   Model)),
    writeln("Training SVM complete!").

%%%%%%%%%%%%% sub-sampling %%%%%%%%%%%%%%
subsample(_, _, [], []):-
    !.
subsample(Pos_Prop, Neg_Prop, [D-1 | In_Data], [D-1 | Out_Data]):-
    random(R), R =< Pos_Prop,
    subsample(Pos_Prop, Neg_Prop, In_Data, Out_Data), !.
subsample(Pos_Prop, Neg_Prop, [_-1 | In_Data], Out_Data):-
    subsample(Pos_Prop, Neg_Prop, In_Data, Out_Data), !.
subsample(Pos_Prop, Neg_Prop, [D-0 | In_Data], [D-0 | Out_Data]):-
    random(R), R =< Neg_Prop,
    subsample(Pos_Prop, Neg_Prop, In_Data, Out_Data), !.
subsample(Pos_Prop, Neg_Prop, [_-0 | In_Data], Out_Data):-
    subsample(Pos_Prop, Neg_Prop, In_Data, Out_Data), !.

