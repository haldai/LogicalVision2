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

/* translation between vector of point coordinates and prolog list
 * @term: prolog term of list, [[x1, y1, z1], [x2, y2, z2], ...]
 * @list: vector of scalar, each scalar is a point coordinate
 */
vector<Scalar> point_list2vec(PlTerm term);
PlTerm point_vec2list(vector<Scalar> list);

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

#endif
