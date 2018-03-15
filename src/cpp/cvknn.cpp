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
#include <mlpack/methods/neighbor_search/neighbor_search.hpp>

using namespace mlpack::neighbor;
using namespace std;
using namespace arma;
using namespace mlpack;
using namespace mlpack::metric; 


/* knn(+List_of_Insts, +Num_Clusters, -Neighbors, -Distance)
 * @List_of_Insts: [Inst1, Inst2, ...] list of list
 * @Num_clusters: number of clusters
 * @Neighbors: Neighbors of each instance [C1, C2, ...].
 * @Distance: Distance from each Neighbor
 */
PREDICATE(knn, 4) {
    vector<vector<double>> insts = list2vecvec<double>(A1);
    size_t nrows = insts[0].size();
    size_t ncols = insts.size();
    mat data(nrows, ncols);
    // put points into data
    for (size_t i = 0; i < ncols; i++)
        data.col(i) = Col<double>(insts[i]);
   
    NeighborSearch<NearestNeighborSort, ManhattanDistance> knnModel(data);
    
    arma::Mat<size_t> result_nb;
    arma::mat result_Dist;
    knnModel.Search(5, result_nb, result_Dist);
    vector<double> nbor(result_nb.n_elem);
    for (size_t i = 0; i < result_nb.n_elem; ++i) {
    	nbor[i] = result_nb[i];
    }

    A3 = vec2list(nbor);
    A4 = arma_mat2list<double>(result_Dist);
    return TRUE;
}
