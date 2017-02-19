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
#include "statistics.hpp"
#include "memread.hpp"
#include "errors.hpp"
#include "utils.hpp"

#include <opencv2/core/core.hpp>
#include <opencv2/highgui/highgui.hpp>
#include <SWI-cpp.h>
#include <SWI-Prolog.h>
#include <mlpack/core.hpp>
#include <mlpack/methods/kmeans/kmeans.hpp>

using namespace std;

/* train_adaboost(+Data_with_label, +Num_classes, +Bucket_size,
 *                +Iterations, +Tolerance, -Model)
 * @Data_with_label: [X1-Y1, X2-Y2, ...], where Xi = [Xi1, Xi2, ...]
 * @Num_classes: number of classes
 * @Bucket_size/Iterations/Tolerance: parameters for decision stump and adaboost
 * @Model: output model address
 */
PREDICATE(train_adaboost, 6) {
    arma::mat data;
    arma::Row<size_t> labels;
    if (!pl_term_2_data_labels(A1, data, labels)) {
        cout << "Parsing data and labels failed!" << endl;
        return FALSE;
    }
    int num_classes = (int) A2;
    int bucket_size = (int) A3;
    int iterations = (int) A4;
    double tolerance = (double) A5;
    if (num_classes <= 0 ||
        bucket_size <= 0 ||
        iterations  <= 0 ||
        tolerance   <= .0) {
        cout << "Wrong parameters!" << endl;
        return FALSE;
    }
    // train
    try {
        if (!(A6 = pl_train_adaboost(data, labels,
                                     num_classes, bucket_size,
                                     iterations, tolerance))) {
            cout << "Training failed!" << endl;
            return FALSE;
        }
    } catch (...) {
        cout << "Training failed!" << endl;
        return FALSE;
    }
    return TRUE;
}

/* predict_adaboost(+Model, +Data, -Labels)
 * @Model: model address
 * @Data: [[X11, X12, ...], [X21, ...], ...] input data
 * @Labels: [Y1, Y2, ...] output labels
 */
PREDICATE(predict_adaboost, 3) {
    // predict
    char *p1 = (char*) A1;
    const string add_model(p1);
    ADABOOST_DS *model = str2ptr<ADABOOST_DS>(add_model);
    arma::mat data = list_data2arma_mat(A2);
    try {
        if (!(A3 = pl_predict_adaboost(model, data))) {
            cout << "Prediction failed!" << endl;
            return FALSE;
        }
    } catch (...) {
        cout << "Prediction failed!" << endl;
        return FALSE;
    }
    return TRUE;
}

PREDICATE(save_model_ada, 1) {
    return TRUE;
}

PREDICATE(load_model_ada, 2) {
    return TRUE;
}

PREDICATE(release_model_ada, 1) {
    char *p1 = (char*) A1;
    const string add_model(p1);
    ADABOOST_DS *model = str2ptr<ADABOOST_DS>(add_model);
    delete model;
    cout << "Released AdaBoost model: " << add_model << endl;
    return TRUE;
}

/* train_svm(+Data_with_label, '-Opt1 Val1 -Opt2 Val2...', -Model)
 * @Data_with_label: [X1-Y1, X2-Y2, ...], where Xi = [Xi1, Xi2, ...]
 * @Parameters are inputed same as libSVM:
 *  -s svm_type : set type of SVM (default 0)
 *     0 -- C-SVC
 *     1 -- nu-SVC
 *     2 -- one-class SVM
 *     3 -- epsilon-SVR
 *     4 -- nu-SVR
 *  -t kernel_type : set type of kernel function (default 2)
 *     0 -- linear: u'*v
 *     1 -- polynomial: (gamma*u'*v + coef0)^degree
 *     2 -- radial basis function: exp(-gamma*|u-v|^2)
 *     3 -- sigmoid: tanh(gamma*u'*v + coef0)
 *  -d degree : set degree in kernel function (default 3)
 *  -g gamma : set gamma in kernel function (default 1/num_features)
 *  -r coef0 : set coef0 in kernel function (default 0)
 *  -c cost : set the parameter C of C-SVC, epsilon-SVR, and nu-SVR (default 1)
 *  -n nu : set the parameter nu of nu-SVC, one-class SVM, and nu-SVR
 *     (default 0.5)
 *  -p epsilon : set the epsilon in loss function of epsilon-SVR (default 0.1)
 *  -m cachesize : set cache memory size in MB (default 100)
 *  -e epsilon : set tolerance of termination criterion (default 0.001)
 *  -h shrinking: whether to use the shrinking heuristics, 0 or 1 (default 1)
 *  -b probability_estimates: whether to train a SVC or SVR model for
 *     probability estimates, 0 or 1 (default 0)
 *  -wi weight: set the parameter C of class i to weight*C, for C-SVC
 *      (default 1)
 * @Model: output model address
 */
PREDICATE(train_svm, 3) {
    // parse data
    arma::mat data;
    arma::Row<size_t> labels;
    if (!pl_term_2_data_labels(A1, data, labels)) {
        cout << "Parsing data and labels failed!" << endl;
        return FALSE;
    }
    // parse parameters
    struct svm_parameter param = pl_parse_svm_parameter(A2);
    // train
    try {
        if (!(A3 = pl_train_svm(data, labels, param))) {
            cout << "Training failed!" << endl;
            return FALSE;
        }
    } catch (...) {
        cout << "Training failed!" << endl;
        return FALSE;
    }
    return TRUE;
}

/* predict_svm(+Model, +Data, -Labels)
 * @Model: model address
 * @Data: [[X11, X12, ...], [X21, ...], ...] input data
 * @Labels: [Y1, Y2, ...] output labels
 */
PREDICATE(predict_svm, 3) {
    try {
        // predict
        char *p1 = (char*) A1;
        const string add_model(p1);
        SVM *model = str2ptr<SVM>(add_model);
        arma::mat data = list_data2arma_mat(A2);
        if (!(A3 = pl_predict_svm(model, data))) {
            cout << "Prediction failed!" << endl;
            return FALSE;
        }
    } catch (...) {
        cout << "Prediction failed!" << endl;
        return FALSE;
    }
    return TRUE;
}

/* save_model_svm(+Model, +Path)
 * save libSVM model into a binary file
 * @Model: Model address
 * @Path: File path for saving the model
 */
PREDICATE(save_model_svm, 2) {
    char *p1 = (char*) A1;
    const string add_model(p1);
    SVM *model = str2ptr<SVM>(add_model);
    char *p2 = (char*) A2;
    const string path(p2);
    try {
        model->save_model(path);
    } catch (...) {
        cout << "Saving SVM model failed!" << endl;
        return FALSE;
    }
    cout << "SVM model saved to: " << path << endl;
    return TRUE;
}

/* load_model_svm(+Path, -Model)
 * load libSVM model from a binary file
 * @Path: File path for saving the model
 * @Model: Model address 
 */
PREDICATE(load_model_svm, 2) {
    char *p1 = (char*) A1;
    const string path(p1);
    SVM *model;
    try {
         model = new SVM(path); // call constructor by loading
    } catch (...) {
        cout << "Loading SVM model failed!" << endl;
        return FALSE;
    }
    cout << "SVM model loaded from: " << path << endl;
    string add = ptr2str(model);
    return A2 = PlTerm(add.c_str());
}

PREDICATE(release_model_svm, 1) {
    char *p1 = (char*) A1;
    const string add_model(p1);
    SVM *model = str2ptr<SVM>(add_model);
    delete model;
    cout << "Released SVM: " << add_model << endl;
    return TRUE;
}

PREDICATE(test_stat, 2) {
    arma::mat data;
    mlpack::data::Load("test_nonlinsep.txt", data);
    arma::Mat<size_t> labels;
    mlpack::data::Load("test_labels_nonlinsep.txt", labels);
    struct svm_parameter param = pl_parse_svm_parameter(A1);
    PlTerm model_term = pl_train_svm(data, labels, param);
    char *p1 = (char*) model_term;
    const string add_model(p1);
    SVM *model = str2ptr<SVM>(add_model);
    A2 = pl_predict_svm(model, data);
    return TRUE;
}

/* compare_hist(+HIST_1, +HIST_2, -DIST)
 * compare 2 color histograms of two sets of points to decide whether
 * their distribution is identical.
 * @HIST_1/2: two HISTOGRAMS
 * @DIST: distance of histograms (quadratic mean of KL divergence
 *        in 3 channels).
 */
PREDICATE(compare_hist, 3) {
    // get hists
    vector<double> h1 = list2vec<double>(A1);
    vector<double> h2 = list2vec<double>(A2);
    // calculate histogram difference
    double d = compare_hist(h1, h2);
    return A3 = d;
}
