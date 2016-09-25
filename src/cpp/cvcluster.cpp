#include "utils.hpp"

#include <SWI-cpp.h>
#include <SWI-Prolog.h>
#include <mlpack/core.hpp>
#include <mlpack/methods/kmeans/kmeans.hpp>

using namespace std;
using namespace arma;
using namespace mlpack;

/* kmeans(+List_of_Insts, +Num_Clusters, -Clusters, -Assignments)
 * @List_of_Insts: [Inst1, Inst2, ...] list of list
 * @Num_clusters: number of clusters
 * @Clusters: [C1, C2, ...], C = [Inst1, Inst2, ...].
 * @Assignments: assignments of each instance [C1, C2, ...].
 */
PREDICATE(kmeans, 4) {
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
    // make new term for groups
    return TRUE;
}
