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


//! \addtogroup fn_sparse_to_dense
//! @{
#ifndef _FN_SPARSE_TO_DENSE_HPP
#define _FN_SPARSE_TO_DENSE_HPP

#include <armadillo>
#include "svm.h"

//!\brief
//! - Constructs a dense Armadillo matrix from a LIBSVM sparse representation matrix.
//!
//! - Input Arguments:
//!   - X      : LIBSVM's sparse representation matrix.
//!   - n_rows : Number of X's rows.
//!   - n_cols : Number of X's columns.
//!
//! - Output Arguments:
//!   - Void.
//!
//! - Return Argument:
//!   - Dense representation of input sparse matrix X.
arma::mat
sparse_to_dense
  (
	struct svm_node** const X,
	const u32 n_rows,
	const u32 n_cols
  )
  {
  // allocate space for dense matrix representation
  arma::mat dense_matrix = arma::zeros<arma::mat>(n_rows, n_cols);
  
  // for each row of X
  for(u32 i = 0; i < n_rows; i++)
    {  
    struct svm_node* tmp_node = const_cast<struct svm_node*>(X[i]);
    
    while(tmp_node->index != -1)
      {
      dense_matrix(i, tmp_node->index - 1) = tmp_node->value;
      tmp_node++;
      }
      
    }
 
  return dense_matrix; 
  }


#endif
//! @}
