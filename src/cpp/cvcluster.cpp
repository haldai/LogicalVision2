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
#include "utils.hpp"

#include <SWI-cpp.h>
#include <SWI-Prolog.h>
#include <mlpack/core.hpp>
#include <mlpack/methods/kmeans/kmeans.hpp>

using namespace std;
using namespace arma;
using namespace mlpack;

/* kmeans(+List_of_Insts, +Num_Clusters, -Clusters, -Assignments, -Centroids)
 * @List_of_Insts: [Inst1, Inst2, ...] list of list
 * @Num_clusters: number of clusters
 * @Clusters: [C1, C2, ...], C = [Inst1, Inst2, ...].
 * @Assignments: assignments of each instance [C1, C2, ...].
 */
PREDICATE(kmeans, 5) {
    vector<vector<double>> insts = list2vecvec<double>(A1);
    int num_clusters = (int) A2;
    size_t nrows = insts[0].size();
    size_t ncols = insts.size();
    mat data(nrows, ncols);
    // put points into data
    for (size_t i = 0; i < ncols; i++)
        data.col(i) = Col<double>(insts[i]);
    Row<size_t> assignments; // Cluster assignments.
    mat centroids; // Cluster centroids
    kmeans::KMeans<> kmeans;
    kmeans.Cluster(data, num_clusters, assignments, centroids);
    vector<long> assign;
    for (size_t i = 0; i < ncols; i++)
        assign.push_back((long) assignments(i));
    A4 = vec2list(assign);
    A3 = group2lists(insts, num_clusters, assignments);
    A5 = arma_mat2list<double>(centroids);
    // make new term for groups
    return TRUE;
}
