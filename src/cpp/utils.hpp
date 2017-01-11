/* Utils module
 * ================================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

#ifndef _UTILS_HPP
#define _UTILS_HPP

//#include <armadillo>
#include <mlpack/core.hpp>
#include <opencv2/core/core.hpp>
#include <SWI-cpp.h>
#include <SWI-Prolog.h>

#include <iostream> // for standard I/O
#include <string>   // for strings

using namespace std;
using namespace cv;

/********** declaration **********/

/* get element address in a vector
 * @*vec: address of vector
 * @idx: index of element
 */
template <class Type>
Type *element_ptr_in_vector(const vector<Type> *vec, int idx);

/* overload the + and += operation of std::vector
 */
template <typename T>
std::vector<T> operator+(const std::vector<T> &A, const std::vector<T> &B);
template <typename T>
std::vector<T> &operator+=(std::vector<T> &A, const std::vector<T> &B);

/* parse prolog key-value map   
 */
bool term_key_value(const PlTerm & term, PlTerm & key, PlTerm & value);

/* list2vec
 * translate a PlTerm (list) into vector
 * @Type: c-type of terms in the list (int, long, double, wchar*, char*),
 *   CANNOT handle string
 * @PlTerm: input term
 * @size: number of elements in list, -1 means all elements
 */
template <class Type>
vector<Type> list2vec(const PlTerm & term, int size = -1);
template <class Type>
PlTerm vec2list(const vector<Type> & list, int size = -1);
template <class Type>
vector<vector<Type>> list2vecvec(const PlTerm & term, int size_outer = -1,
                                 int size_inner = -1);
template <class Type>
PlTerm vecvec2list(const vector<vector<Type>> & vec, int size_outer = -1,
                   int size_inner = -1);
template <class Type>
PlTerm arma_mat2list(const arma::mat & m, int size_outer = -1,
                     int size_inner = -1);
// prolog list of list to arma data, so all the elements have to be double
arma::mat list_data2arma_mat(const PlTerm & pldata);

/* Return an empty list */
PlTerm empty_list();

/* transformation between 3D-scalar vector and prolog list */
template <class Type>
vector<Scalar> scalar_list2vec(const PlTerm & term, int size = -1, int dim = 3);
template <class Type>
PlTerm scalar_vec2list(const vector<Scalar> & list, int size = -1, int dim = 3);

/* transformation between vector of point coordinates and prolog list
 * @term: prolog term of list, [[x1, y1, z1], [x2, y2, z2], ...]
 * @list: vector of scalar, each scalar is a point coordinate
 */
vector<Scalar> point_list2vec(const PlTerm & term, int size = -1, int dim = 3);
PlTerm point_vec2list(const vector<Scalar> & list, int size = -1, int dim = 3);

/* reassign data points into lists of groups according to clustering results
 * @points: data points (vector of feature vectors)
 * @group_num: number of groups
 * @assignments: clustering assignments of data points
 * @returned: a Prolog term, list of groups of points. L = [[Grp1], ...],
 *    Grp = [Pt1, ...], Pt = [x1, x2, ...].
 */
PlTerm group2lists(const vector<vector<double>> & points, int group_num, arma::Row<size_t> assignments);

/* Assert and retract a fact (PlCompoud as a PlTerm) in Prolog Engine */
void pl_assert(string pred, PlTerm args);
void pl_retract(string pred, PlTerm args);

/********* implementation ********/
template <class Type>
Type *element_ptr_in_vector(const vector<Type> *vec, int idx) {
    if (idx < 0 || idx >= (int) vec->size()) {
        cerr << "Element number out of bound!" << endl;
        return NULL;
    }
    return (Type *) &((*vec)[idx]);
}

bool term_key_value(const PlTerm & term, PlTerm & key, PlTerm & value) {
    std::string pred(term.name());
    if (pred.compare("-"))
        return false;
    key = term[1];
    value = term[2];
    return true;
}

template <class Type>
vector<Type> list2vec(const PlTerm & term, int size) {
    static_assert((is_same<Type, int>::value)
                  || (is_same<Type, long>::value)
                  || (is_same<Type, double>::value)
                  || (is_same<Type, wchar_t*>::value)
                  || (is_same<Type, char*>::value),
                  "Wrong template type for list2vec!");
    vector<Type> re;
    try {
        PlTail tail(term);
        PlTerm e;
        if (size == -1) {
            while(tail.next(e))
                re.push_back((Type) e);
        } else {
            for (int i = 0; i < size; i++) {
                tail.next(e);
                re.push_back((Type) e);
            }
        }
        return re;
    } catch (...) {
        return re;
    }
}
template <class Type>
PlTerm vec2list(const vector<Type> & list, int size) {
    static_assert((is_same<Type, long>::value)
                  || (is_same<Type, double>::value)
                  || (is_same<Type, wchar_t*>::value)
                  || (is_same<Type, char*>::value)
                  || (is_same<Type, PlTerm>::value),
                  "Wrong template type for list2vec!");
    term_t term_ref = PL_new_term_ref();
    PlTerm term(term_ref);
    PlTail tail(term);

    try {
        if (size == -1) {
            for (auto it = list.begin(); it != list.end(); ++it)
                tail.append((Type) *it);
            tail.close();
        } else {
            for (int i = 0; i < size; i++)
                tail.append((Type) list[i]);
            tail.close();
        }
        return term;
    } catch (...) {
        cerr << "Saving list error!" << endl;
        return term;
    }
}

template <class Type>
vector<vector<Type>> list2vecvec(const PlTerm & term,
                                 int size_outer, int size_inner) {
    static_assert((is_same<Type, int>::value)
                  || (is_same<Type, long>::value)
                  || (is_same<Type, double>::value)
                  || (is_same<Type, wchar_t*>::value)
                  || (is_same<Type, char*>::value),
                  "Wrong template type for list2vec!");
    assert(term.type() != PL_VARIABLE);

    vector<vector<Type>> re;
    try {
        PlTail tail(term);
        PlTerm e;
        if (size_outer == -1) {
            while(tail.next(e)) {
                vector<Type> p = list2vec<Type>(e, size_inner);
                re.push_back(p);
            }
        } else {
            for (int i = 0; i < size_outer; i++) {
                tail.next(e);
                vector<Type> p = list2vec<Type>(e, size_inner);
                re.push_back(p);
            }
        }
    } catch (...) {
        cerr << "Parsing list error!" << endl;
    }
    return re;
}

template <class Type>
PlTerm vecvec2list(const vector<vector<Type>> & vec,
                   int size_outer, int size_inner) {
    static_assert((is_same<Type, long>::value)
                  || (is_same<Type, double>::value)
                  || (is_same<Type, wchar_t*>::value)
                  || (is_same<Type, char*>::value)
                  || (is_same<Type, PlTerm>::value),
                  "Wrong template type for list2vecvec!");
    term_t re_ref = PL_new_term_ref();
    PlTerm re_term(re_ref);
    PlTail re_tail(re_term);

    try {
        size_outer = (size_outer < 0 || size_outer > vec.size()) ?
            vec.size() : size_outer;
        for (int o_idx = 0; o_idx < size_outer; o_idx++) {
            size_inner = (size_inner < 0) ? vec[o_idx].size() : size_inner;
            term_t points_ref = PL_new_term_ref();
            PlTerm points_term(points_ref);
            PlTail points_tail(points_term);
            for (int i_idx = 0; i_idx < size_inner; i_idx++) {
                PlTerm point((Type) (vec[o_idx][i_idx]));
                points_tail.append(point);
            }
            points_tail.close();
            re_tail.append(points_term);
        }
        re_tail.close();
    } catch (...) {
        cerr << "Parsing list error!" << endl;
    }
    return re_term;
}

template <class Type>
PlTerm arma_mat2list(const arma::mat & m, int size_outer, int size_inner) {
    static_assert((is_same<Type, long>::value)
                  || (is_same<Type, double>::value)
                  || (is_same<Type, wchar_t*>::value)
                  || (is_same<Type, char*>::value)
                  || (is_same<Type, PlTerm>::value),
                  "Wrong template type for list2vecvec!");
    term_t re_ref = PL_new_term_ref();
    PlTerm re_term(re_ref);
    PlTail re_tail(re_term);
    try {
        size_outer = (size_outer < 0 ||
                      arma::uword(size_outer) > m.n_cols) ?
            m.n_cols : size_outer;
        for (int o_idx = 0; o_idx < size_outer; o_idx++) {
            size_inner = (size_inner < 0 ||
                          arma::uword(size_inner) > m.col(o_idx).n_rows) ?
                m.col(o_idx).n_rows : size_inner;
            term_t points_ref = PL_new_term_ref();
            PlTerm points_term(points_ref);
            PlTail points_tail(points_term);
            for (int i_idx = 0; i_idx < size_inner; i_idx++) {
                PlTerm point((Type) (m(i_idx, o_idx)));
                points_tail.append(point);
            }
            points_tail.close();
            re_tail.append(points_term);
        }
        re_tail.close();
    } catch (...) {
        cerr << "Parsing list error!" << endl;
    }
    return re_term;
}

arma::mat list_data2arma_mat(const PlTerm & pldata) {
    arma::mat re;
    vector<vector<double>> re_vec;
    try {
        PlTail data(pldata);
        PlTerm instance;
        while(data.next(instance)) {
            PlTail feature(instance);
            PlTerm e;
            vector<double> inst;
            while (feature.next(e))
                inst.push_back((double) e);
            re_vec.push_back(inst);
        }
    } catch (...) {
        cout << "Parsing data to arma::mat failed!" << endl;
        return re;
    }
    int n = re_vec.size();
    int m = re_vec[0].size();
    re = arma::mat(m, n);
    try {
        for (int i = 0; i < n; i++)
            re.col(i) = arma::vec(re_vec[i]);
    } catch (...) {
        cout << "Cannot init data matrix, check the length of feature vectors."
             << endl;
        re.zeros();
        return re;
    }
    return re;
}

PlTerm empty_list() {
    PlTerm re;
    PlTail t(re);
    t.close();
    return re;
}

template <class Type>
PlTerm scalar_vec2list(const vector<Scalar> & list, int size, int dim) {
    static_assert((is_same<Type, long>::value)
                  || (is_same<Type, double>::value),
                  "Wrong template type for list2vec!");
    // make a list of points
    term_t term_ref = PL_new_term_ref();
    PlTerm term(term_ref); // return a PlTerm, not PlTail
    PlTail term_list(term);
    try {
        // insert points
        if (size < 0)
            size = list.size();
        //for (auto it = list.begin(); it != list.end(); ++it) {
        for (int i = 0; i < size; i++) {
            //Scalar pt = *it;
            Scalar pt = list[i];
            // make a list of coordinates
            term_t point_ref = PL_new_term_ref();
            PlTerm point_term(point_ref); // PlTerm for insertion
            PlTail coord_list(point_term);
            for (int dim_ = 0; dim_ < dim; dim_++)
                coord_list.append((Type) pt[dim_]);
            coord_list.close();
            term_list.append(point_term);
        }
        term_list.close();
    } catch (...) { 
        cerr << "[scalar_vec2list] Create prolog list failed!" << endl;
    }
    return term;
}

template <class Type>
vector<Scalar> scalar_list2vec(const PlTerm & term, int size, int dim) {
    static_assert((is_same<Type, int>::value)
                  || (is_same<Type, long>::value)
                  || (is_same<Type, double>::value),
                  "Wrong template type for list2vec!");
    vector<Scalar> vec;
    try {
        PlTail term_list(term);
        PlTerm point_term;
        if (size < 0) {
            while (term_list.next(point_term)) {
                PlTail coord_list(point_term);
                PlTerm coord_term;
                Scalar pt(-1, -1, -1);
                for (int dd = 0; dd < dim; dd++) {
                    coord_list.next(coord_term);
                    pt[dd] = (Type) coord_term;
                }
                vec.push_back(pt);
            }
        } else {
            for (int i = 0; i < size; i++) {
                if (!term_list.next(point_term))
                    break;
                PlTail coord_list(point_term);
                PlTerm coord_term;
                Scalar pt(-1, -1, -1);
                for (int dd = 0; dd < dim; dd++) {
                    coord_list.next(coord_term);
                    pt[dd] = (Type) coord_term;
                }
                vec.push_back(pt);                
            }
        }
    } catch (...) {
        cerr << "[point_list2vec] Recieving coordinates from prolog list error !" << endl;
    }
    return vec;
}

PlTerm point_vec2list(const vector<Scalar> & list, int size, int dim) {
    return scalar_vec2list<long>(list, size, dim);
}

vector<Scalar> point_list2vec(const PlTerm & term, int size, int dim) {
    return scalar_list2vec<int>(term, size, dim);
}

template <typename T>
std::vector<T> operator+(const std::vector<T> &A, const std::vector<T> &B) {
    std::vector<T> AB;
    AB.reserve(A.size() + B.size());                // preallocate memory
    AB.insert(AB.end(), A.begin(), A.end());        // add A;
    AB.insert(AB.end(), B.begin(), B.end());        // add B;
    return AB;
}

template <typename T>
std::vector<T> &operator+=(std::vector<T> &A, const std::vector<T> &B) {
    // preallocate memory without erase original data
    A.reserve(A.size() + B.size());
    A.insert(A.end(), B.begin(), B.end()); // add B;
    return A;
}

void pl_assert(string pred, PlTerm args) {
    PlTermv pl_args(1);
    pl_args[0] = PlCompound(pred.c_str(), args);
    PlQuery q("assertz", pl_args);
    q.next_solution();
}

void pl_retract(string pred, PlTerm args) {
    PlTermv pl_args(1);
    pl_args[0] = PlCompound(pred.c_str(), args);
    PlQuery q("retract", pl_args);
    q.next_solution();
}

PlTerm group2lists(const vector<vector<double>> & items,
                   int group_num,
                   arma::Row<size_t> assignments) {
    //[[seg1,seg2],[seg3,seg4],...], where each seg=[x1,x2,...]
    // assign items to groups
    vector<vector<vector<double>>> groups; // groups<items<features>>
    for (int i = 0; i < group_num; i++)
        groups.push_back(vector<vector<double>>());
    for (size_t i = 0; i < assignments.n_cols; i++) {
        size_t cls = assignments(i);
        groups[cls].push_back(items[i]);
    }
    // make new term for groups
    term_t groups_ref = PL_new_term_ref();
    PlTerm groups_term(groups_ref);
    PlTail groups_tail(groups_term);
    for (int grp = 0; grp < group_num; grp++) {
        term_t items_ref = PL_new_term_ref();
        PlTerm items_term(items_ref);
        PlTail items_tail(items_term);
        size_t items_num = groups[grp].size();
        for (size_t pt = 0; pt < items_num; pt++) {
            PlTerm item = vec2list<double>(groups[grp][pt]);
            items_tail.append(item);
        }
        items_tail.close();
        groups_tail.append(items_term);
    }
    groups_tail.close();
    return groups_term;    
}

#endif
