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
// Copyright (C) 2011 the authors listed below
// http://ecocpak.sourceforge.net
// 
// Authors:
// - Dimitrios Bouzas (bouzas at ieee dot org)
// - Nikolaos Arvanitopoulos (niarvani at ieee dot org)
// - Anastasios Tefas (tefas at aiia dot csd dot auth dot gr)
// 
// This file is part of the ECOC PAK C++ library. It is 
// provided without any warranty of fitness for any purpose.
//
// You can redistribute this file and/or modify it under 
// the terms of the GNU Lesser General Public License (LGPL) 
// as published by the Free Software Foundation, either 
// version 3 of the License or (at your option) any later 
// version.
// (see http://www.opensource.org/licenses for more info)


//! \addtogroup fn_dense_to_sparse
//! @{
#ifndef _FN_DENSE_TO_SPARSE_HPP
#define _FN_DENSE_TO_SPARSE_HPP

#include <armadillo>
#include "svm.h"

#define Malloc(type, n) (type *)malloc( ( n ) * sizeof( type ) )

// alias to svm_node**
typedef struct svm_node** sparse_mat;
typedef arma::size_t u32;



//! \brief
//! - Constructs a sparse LIBSVM matrix from a dense Armadillo representation matrix.
//!
//! - Input Arguments:
//!   - X : Armadillo's dense matrix.
//!
//! - Output Arguments:
//!   - Void.
//!
//! - Return Argument:
//!   - Sparse representation of input sparse matrix X.
sparse_mat
dense_to_sparse
  (
  const arma::mat& X
  )
  {
  // number of rows
  const u32 n_rows = X.n_rows;
  
  // sparse matrix X  
  struct svm_node** sparse_matrix = Malloc(struct svm_node *, n_rows);
  
  // for each row
  for(arma::size_t i = 0; i < n_rows; i++)
    {
    // indices of non zero elements
    arma::ucolvec inds = find(X.row(i));
    
    // number of non zero elements
    const u32 n_elem = inds.n_elem;
    
    // allocate space for sparse row
    struct svm_node* x_space = Malloc(struct svm_node, n_elem + 1);
    
    // set last row element of sparse representation
    x_space[n_elem].index = -1;
    x_space[n_elem].value = 0;
    
    // fill in the rest of the elements
    for(u32 j = 0; j < n_elem; j++)
      {
      x_space[j].index = inds[j];
      x_space[j].value = X(i, inds[j]);
      }
      
    sparse_matrix[i] = x_space;
    }

  return sparse_matrix; 
  }



//! \brief
//! 
//!
void
delete_sparse_matrix(sparse_mat A, const u32 n_rows)
  {
  // free sparse matrix
  if(A != 0)
    {
    for(arma::size_t i = 0; i < n_rows; i++)
      {
      svm_node* tmp_node = A[i];
      
      if(tmp_node != 0)
        {
        delete [] tmp_node;
        }
        
      }
      
    delete [] A;
    }
  
  }



//! \brief
//! 
//!
void
delete_sparse_matrix(sparse_mat A, const u32 n_rows, const struct svm_model* model)
  {
  // free sparse matrix
  if(A != 0)
    {
    
    for(arma::size_t i = 0; i < n_rows; i++)
      {
      //bool erase =  true;
      
      //for(u32 j = 0; j < model->l; j++)
      //  {
      //  if(model->SV[j] == A[i])
      //    {
      //    erase = false;
      //    break;
      //    }
          
     //   }
      
    //  if(erase == true)
    //    {
        svm_node* tmp_node = A[i];
        
    //    if(tmp_node != 0)
    //      {
          free(tmp_node);
    //      }
          
     //   }
        
      }
      
    free(A);
    }
    
  }



svm_model
modelcpy(svm_model* m)
  {
  // output LIBSVM model
  svm_model out;
   
  // copy svm parameters
  out.param = m->param;
   
  // copy number of classes
  out.nr_class = m->nr_class;
   
  // copy total number of support vectors
  out.l = m->l;
   
  // copy free sentinel
  out.free_sv = m->free_sv;
 
  // copy label for each class
  out.label = new int[2];
  out.label[0] = m->label[0];
  out.label[1] = m->label[1];
   
  // copy number of support vectors for each class
  out.nSV = new int[2];
  out.nSV[0] = m->nSV[0];
  out.nSV[1] = m->nSV[1]; 

  // copy coefficients of support vectors in decision functions
  out.sv_coef = new double*[1];
  out.sv_coef[0] = new double[out.l];
  memcpy(out.sv_coef[0], m->sv_coef[0], out.l * sizeof(double));	 
  
  // copy biases in decision functions -- one rho since 2 classes
  out.rho = new double[1];
  out.rho[0] = m->rho[0];
   
  // --- copy support vectors --- //
   
  out.SV = new svm_node*[out.l];
     
  for(int i = 0; i < out.l; i++)
    {
	vector<svm_node> tmpSV;
	svm_node tmp = m->SV[i][0];
	
	u32 k = 0;
	while(tmp.index != -1)
	  {
      tmpSV.push_back(tmp);
	  k++;
	  
	  tmp = m->SV[i][k];
	  }
	  
	out.SV[i] = new svm_node[tmpSV.size() + 1]; 
	memcpy(out.SV[i], &tmpSV[0], tmpSV.size() * sizeof(svm_node));
	out.SV[i][tmpSV.size()].index = -1;
	}
  
  // copy pairwise probabilities information
  if(m->probA != NULL)
    {
    out.probA = new double[1];
    out.probA[0] = m->probA[0];
    }
  else
    {
    out.probA = 0;
    }
    
  if(m->probB != NULL)
    {  
    out.probB = new double[1];
    out.probB[0] = m->probB[0];
    }
  else
    {
	out.probB = 0;	
	}
	
  return out;    
  }


#endif
//! @}
