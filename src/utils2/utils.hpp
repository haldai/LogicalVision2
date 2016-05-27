/* Utils module
 * ================================
 * Version: 2.0
 * Author: Wang-Zhou Dai <dai.wzero@gmail.com>
 */

#ifndef _UTILS_HPP
#define _UTILS_HPP

#include <opencv2/core/core.hpp> // opencv library
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

/* list2vec
 * translate a PlTerm (list) into vector
 * @Type: c-type of terms in the list (int, long, double, wchar*, char*),
 *   CANNOT handle string
 * @PlTerm: input term
 * @size: number of elements in list, -1 means all elements
 */
template <class Type>
vector<Type> list2vec(PlTerm term, int size = -1);
template <class Type>
PlTerm vec2list(vector<Type> list, int size = -1);
template <class Type>
vector<vector<Type>> list2vecvec(PlTerm term, int size_outer = -1,
                                 int size_inner = -1);
template <class Type>
PlTerm vecvec2list(vector<vector<Type>> vec, int size_outer = -1,
                   int size_inner = -1);

/* Return an empty list */
PlTerm empty_list();

/* translation between vector of point coordinates and prolog list
 * @term: prolog term of list, [[x1, y1, z1], [x2, y2, z2], ...]
 * @list: vector of scalar, each scalar is a point coordinate
 */
vector<Scalar> point_list2vec(PlTerm term);
PlTerm point_vec2list(vector<Scalar> list);

/* Assert and retract a fact (PlCompoud as a PlTerm) in Prolog Engine */
void pl_assert(string type, PlTerm A);
void pl_retract(string type, PlTerm A);

/********* implementation ********/
template <class Type>
Type *element_ptr_in_vector(const vector<Type> *vec, int idx) {
    if (idx < 0 || idx >= (int) vec->size()) {
        cerr << "Element number out of bound!" << endl;
        return NULL;
    }
    return (Type *) &((*vec)[idx]);
}

template <class Type>
vector<Type> list2vec(PlTerm term, int size) {
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
PlTerm vec2list(vector<Type> list, int size) {
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
vector<vector<Type>> list2vecvec(PlTerm term, int size_outer, int size_inner) {
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
PlTerm vecvec2list(vector<vector<Type>> vec, int size_outer, int size_inner) {
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
        size_outer = (size_outer < 0) ? vec.size() : size_outer;
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

PlTerm empty_list() {
    PlTerm re;
    PlTail t(re);
    t.close();
    return re;
}

PlTerm point_vec2list(vector<Scalar> list) {
    // make a list of points
    term_t term_ref = PL_new_term_ref();
    PlTerm term(term_ref); // return a PlTerm, not PlTail
    PlTail term_list(term);
    try {
        // insert points
        for (auto it = list.begin(); it != list.end(); ++it) {
            Scalar pt = *it;
            // make a list of coordinates
            term_t point_ref = PL_new_term_ref();
            PlTerm point_term(point_ref); // PlTerm for insertion
            PlTail coord_list(point_term);
            for (int dim = 0; dim < 3; dim++)
                coord_list.append((long) pt[dim]);
            coord_list.close();
            term_list.append(point_term);
        }
        term_list.close();
    } catch (...) { 
        cerr << "[point_vec2list] Create prolog list failed!" << endl;
    }
    return term;
}

vector<Scalar> point_list2vec(PlTerm term) {
    vector<Scalar> vec;
    try {
        PlTail term_list(term);
        PlTerm point_term;
        while (term_list.next(point_term)) {
            PlTail coord_list(point_term);
            PlTerm coord_term;
            Scalar pt(-1, -1, -1);
            for (int dim = 0; dim < 3; dim++) {
                coord_list.next(coord_term);
                pt[dim] = (int) coord_term;
            }
            vec.push_back(pt);
        }
    } catch (...) {
        cerr << "[point_list2vec] Recieving coordinates from prolog list error !" << endl;
    }
    return vec;
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

void pl_assert(string type, PlTerm A) {
    PlTermv pl_args(1);
    pl_args[0] = PlCompound(type.c_str(), A);
    PlQuery q("assert", pl_args);
    q.next_solution();
}

void pl_retract(string type, PlTerm A) {
    PlTermv pl_args(1);
    pl_args[0] = PlCompound(type.c_str(), A);
    PlQuery q("retract", pl_args);
    q.next_solution();
}

#endif
