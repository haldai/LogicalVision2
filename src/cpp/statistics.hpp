/* statistical feature extractor module
 * =========================================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */
#ifndef _STATISTICS_HPP
#define _STATISTICS_HPP

#include <opencv2/core/core.hpp>
#include <opencv2/imgproc.hpp>
#include <opencv2/highgui/highgui.hpp>  // OpenCV window I/O
#include <mlpack/core.hpp>
#include <mlpack/methods/adaboost/adaboost.hpp> // adaboost model
#include <SWI-cpp.h>
#include <SWI-Prolog.h>

#include <iostream> // for standard I/O
#include <cstdlib>
#include <algorithm>
#include <string>   // for strings
#include <cmath>

#include "utils.hpp"
#include "memread.hpp"
#include "svm.h"
#include "fn_dense_to_sparse.hpp"

typedef mlpack::adaboost::AdaBoost<mlpack::decision_stump::DecisionStump<>> ADABOOST_DS;
typedef mlpack::adaboost::AdaBoost<mlpack::perceptron::Perceptron<>> ADABOOST_P;

using namespace std;

/**************** Statistical model classes ****************/
class SVM {
    // libSVM model
public:
    // constructor
    SVM(const SVM & c);
    SVM(const string path); // constructor by loading from file
    SVM(const arma::mat & data,
        const arma::Row<size_t> & labels,
        const struct svm_parameter & param);
    // destructor
    ~SVM();

    // accessor
    struct svm_model get_model();

    // saving model
    void save_model(const string & path);

    // predictor
    vector<double> predict(const arma::mat & data) const;
    double predict(const arma::colvec & instance) const;
private:
    struct svm_model model;
};

struct svm_parameter pl_parse_svm_parameter(PlTerm term);

/*************** statistical operations *************/
/* parse key-value pairs into armadillo dataset
 */
bool pl_term_2_data_labels(const PlTerm & pldata,
                           arma::mat & data,
                           arma::Row<size_t> & labels);

/* train an adaboost model with given training data
 * @data/label: data in armadillo matrix
 * @iterations/tolerance: parameters for adaboost
 * @num_classes/bucket_size: parameters for decision stumps
 * @return: PlTerm of model address
 */
PlTerm pl_train_adaboost(const arma::mat & data,
                         const arma::Row<size_t> & labels,
                         const size_t num_classes = 2, // parameter for ds
                         const size_t bucket_size = 6,
                         const size_t iterations = 50,
                         const double tolerance = 1e-10);
PlTerm pl_train_adaboost_perceptron(const arma::mat & data,
                                    const arma::Row<size_t> & labels,
                                    const size_t num_classes = 2,
                                    const size_t perceptron_iter = 800,
                                    const size_t iterations = 1000,
                                    const double tolerance = 1e-10);
// model predict, return a list of labels
PlTerm pl_predict_adaboost(ADABOOST_DS *model, const arma::mat & data);
PlTerm pl_predict_adaboost_perceptron(ADABOOST_P *model, const arma::mat & data);

/* libsvm */
PlTerm pl_train_svm(const arma::mat & data,
                    const arma::Row<size_t> & labels);
PlTerm pl_predict_svm(SVM *model, const arma::mat & data);

/* Compare two histogram distributions, return KL divergence
 */
double compare_hist(const vector<double> & hist1,
                    const vector<double> & hist2);
/************** Implementations **************/
SVM::SVM(const SVM & c) {
    svm_model *tmp = const_cast<svm_model*>(&(c.model));
    model = modelcpy(tmp);
}

SVM::SVM(const string path) {
    svm_model *tmp = svm_load_model(path.c_str());
    model = modelcpy(tmp);

    // clean up
    delete [] tmp->SV;
    
    for(auto i = 0; i < tmp->nr_class - 1; i++)
        delete [] tmp->sv_coef[i];
    
    delete [] tmp->sv_coef;
    delete tmp->rho;
    delete [] tmp->label;
    delete tmp->probA;
    delete tmp->probB;
    delete tmp->nSV;
    delete tmp;

}

SVM::SVM(const arma::mat & data,
         const arma::Row<size_t> & labels,
         const struct svm_parameter & param) {
    // data conversion
    struct svm_problem problem;
    arma::colvec label_vec = arma::conv_to<arma::colvec>::from(labels);
    label_vec = (label_vec - 0.5)*2;
    problem.x = dense_to_sparse(data.t());
    problem.l = data.n_cols;
    problem.y = label_vec.memptr();

    // train model
    svm_model* m = svm_train(&problem, &param);
    model = modelcpy(m);
    // clean up
    delete_sparse_matrix(problem.x, problem.l);
    delete [] m->SV;
    
    for(auto i = 0; i < m->nr_class - 1; i++)
        delete [] m->sv_coef[i];
    
    delete [] m->sv_coef;
    delete m->rho;
    delete [] m->label;
    delete m->probA;
    delete m->probB;
    delete m->nSV;
    delete m;
}

SVM::~SVM() {
    delete_sparse_matrix(model.SV, model.l);
    
    for(auto i = 0; i < model.nr_class - 1; i++)
        delete [] model.sv_coef[i];
    //delete [] model.SV;
    delete [] model.sv_coef;
    delete model.rho;
    delete model.label;
    delete model.probA;
    delete model.probB;
    delete model.nSV;
    //delete &model;
}

struct svm_model SVM::get_model() {
    return model;
}

void SVM::save_model(const string & path) {
    svm_save_model(path.c_str(), &(this->model));
}

vector<double> SVM::predict(const arma::mat & data) const {
    vector<double> predictions(data.n_cols);
    for (arma::size_t i = 0; i < data.n_cols; i++)
        predictions[i] = (SVM::predict((arma::colvec) data.col(i)));
    return predictions;
}

double SVM::predict(const arma::colvec & instance) const {
    // temporary variable to hold classifier prediction
    double prediction = 0.0;

    // --- convert Armadillo row vector to LIBSVM type format --- //
    
    // indices of non zero elements
    arma::ucolvec ind = find(instance);
    
    // number of non zero elements
    const arma::size_t n_elem = ind.n_elem;
    
    // allocate space for sparse row
    struct svm_node* x_space = new svm_node[n_elem + 1];
    
    // set last row element of sparse representation
    x_space[n_elem].index = -1;
    x_space[n_elem].value = 0;
    
    // fill in the rest of the elements
    for(arma::size_t i = 0; i < n_elem; i++) {
        x_space[i].index = ind[i];
        x_space[i].value = instance[ind[i]];
    }

    // compute prediction
    svm_predict_values(&model, x_space, &prediction);

    // delete temporary sparse representation
    delete [] x_space;

    // return prediction
    return prediction;
}

bool pl_term_2_data_labels(const PlTerm & pldata,
                           arma::mat & data,
                           arma::Row<size_t> & labels) {
    // pldata is a list
    // each element has the form "[X1, X2, ...]-Y"
    PlTail dataset(pldata);
    PlTerm e;
    std::vector<std::vector<double>> vec_data;
    std::vector<size_t> vec_labels;
    while(dataset.next(e)) {
        PlTerm key, val;
        if (!term_key_value(e, key, val))
            return false;
        std::vector<double> vec = list2vec<double>(key);
        vec_data.push_back(vec);
        size_t label = (size_t) ((int) val);
        vec_labels.push_back(label);
    }
    int n = vec_data.size();
    int m = vec_data[0].size();
    data = arma::mat(m, n);
    try {
        for (int i = 0; i < n; i++)
            data.col(i) = arma::vec(vec_data[i]);
        labels = arma::Row<size_t>(vec_labels);
    } catch (...) {
        cout << "Cannot init data matrix, check the length of feature vectors."
             << endl;
        return false;
    }
    return true;
}

PlTerm pl_train_adaboost(const arma::mat & data,
                         const arma::Row<size_t> & labels,
                         const size_t num_classes,
                         const size_t bucket_size,                         
                         const size_t iterations,
                         const double tolerance) {
    mlpack::decision_stump::DecisionStump<> ds(data, labels.row(0),
                                               num_classes, bucket_size);
    
    // Define parameters for AdaBoost.
    ADABOOST_DS *model = new ADABOOST_DS(data, labels.row(0),
                                         ds, iterations, tolerance);
    string model_addr = ptr2str(model);
    return PlTerm(model_addr.c_str());
}

PlTerm pl_predict_adaboost(ADABOOST_DS *model, const arma::mat & data) {
    arma::Row<size_t> predicted;
    model->Classify(data, predicted);
    std::vector<long> pred_vec = arma::conv_to<std::vector<long>>::from(predicted);
    return vec2list<long>(pred_vec);
}

PlTerm pl_train_adaboost_perceptron(const arma::mat & data,
                                    const arma::Row<size_t> & labels,
                                    const size_t num_classes,
                                    const size_t perceptron_iter,
                                    const size_t iterations,
                                    const double tolerance) {
    mlpack::perceptron::Perceptron<> p(data, labels, num_classes,
                                       perceptron_iter);
    
    // Define parameters for AdaBoost.
    ADABOOST_P *model = new ADABOOST_P(data, labels.row(0),
                                               p, iterations, tolerance);
    string model_addr = ptr2str(model);
    return PlTerm(model_addr.c_str());
}

PlTerm pl_predict_adaboost_perceptron(ADABOOST_P *model,
                                      const arma::mat & data) {
    arma::Row<size_t> predicted;
    model->Classify(data, predicted);
    std::vector<long> pred_vec = arma::conv_to<std::vector<long>>::from(predicted);
    return vec2list<long>(pred_vec);
}

PlTerm pl_train_svm(const arma::mat & data,
                    const arma::Row<size_t> & labels,
                    const struct svm_parameter & param) {
    SVM *model = new SVM(data, labels, param);
    // return address
    string model_addr = ptr2str(model);
    return PlTerm(model_addr.c_str());
}

PlTerm pl_predict_svm(SVM *model, const arma::mat & data) {
    vector<double> predictions = model->predict(data);
    //std::vector<double> pred_vec = arma::conv_to<std::vector<double>>::from(predictions);
    vector<long> re(predictions.size());
    for (arma::size_t i = 0; i < predictions.size(); i++) {
        //cout << predictions[i] << " ";
        if (predictions[i] < 0)
            re[i] = 0;
        else
            re[i] = 1;
    }
    return vec2list<long>(re);
}

struct svm_parameter pl_parse_svm_parameter(PlTerm term) {
    svm_parameter param;
    // default values
	param.svm_type = C_SVC;
	param.kernel_type = RBF;
	param.degree = 3;
	param.gamma = 0.5;	// 1/num_features
	param.coef0 = 0;
	param.nu = 0.5;
	param.cache_size = 100;
	param.C = 1;
	param.eps = 1e-3;
	param.p = 0.1;
	param.shrinking = 1;
	param.probability = 0;
	param.nr_weight = 0;
	param.weight_label = NULL;
	param.weight = NULL;

    char *args = (char*) term;
    
    enum { kMaxArgs = 64 };
    int argc = 0;
    char *argv[kMaxArgs];
    char *p2 = strtok(args, " ");
    while (p2 && argc < kMaxArgs - 1) {
        argv[argc++] = p2;
        p2 = strtok(0, " ");
    }
    argv[argc] = 0;

    // parse options
	for(auto i = 1; i < argc; i+=2) {
		switch(argv[i-1][1]) {
            case 's':
				param.svm_type = atoi(argv[i]);
				break;
			case 't':
				param.kernel_type = atoi(argv[i]);
				break;
			case 'd':
				param.degree = atoi(argv[i]);
				break;
			case 'g':
				param.gamma = atof(argv[i]);
				break;
			case 'r':
				param.coef0 = atof(argv[i]);
				break;
			case 'n':
				param.nu = atof(argv[i]);
				break;
			case 'm':
				param.cache_size = atof(argv[i]);
				break;
			case 'c':
				param.C = atof(argv[i]);
				break;
			case 'e':
				param.eps = atof(argv[i]);
				break;
			case 'p':
				param.p = atof(argv[i]);
				break;
			case 'h':
				param.shrinking = atoi(argv[i]);
				break;
			case 'b':
				param.probability = atoi(argv[i]);
				break;
			case 'w':
				++param.nr_weight;
				param.weight_label = (int *)realloc(param.weight_label,sizeof(int)*param.nr_weight);
				param.weight = (double *)realloc(param.weight,sizeof(double)*param.nr_weight);
				param.weight_label[param.nr_weight-1] = atoi(&argv[i-1][2]);
				param.weight[param.nr_weight-1] = atof(argv[i]);
				break;
			default:
				fprintf(stderr,"Unknown option: -%c\n", argv[i-1][1]);
		}
    }
    return param;
}

double compare_hist(const vector<double> & hist1,
                    const vector<double> & hist2) {
    vector<float> h1(hist1.size()), h2(hist1.size());
    for (uint i = 0; i < hist1.size(); i++) {
        h1[i] = static_cast<float>(hist1[i]);
        h2[i] = static_cast<float>(hist2[i]);
    }
    return compareHist(h1, h2, CV_COMP_KL_DIV);
}

#endif
