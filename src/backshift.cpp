#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
// [[Rcpp::plugins("cpp11")]]

arma::vec interest(arma::vec vector,
                       float self_discharge, 
                       List eff, 
                       bool backwards, 
                       bool depreciate){
  
  arma::vec res = arma::zeros<arma::vec>(vector.n_elem);
  
  if (backwards == true) vector = arma::reverse(vector);

  float vec_local_diff = vector[0] - (vector[0] * 
                            Rcpp::as<float>(eff[0]) * 
                            Rcpp::as<float>(eff[1]));
  
  int n = vector.n_elem;
  
  for (int i=1; i < n; ++i){
    if (depreciate == true) {
      res[i] = (vector[i] - vec_local_diff) * pow(1 - self_discharge, i);
    } else {
      res[i] = (vector[i] + vec_local_diff) / pow(1 - self_discharge, i);
    }
  }
 
  res[0] = vector[0];
  if (backwards == true) res = arma::reverse(res);
  return res;
}

// [[Rcpp::export]]
arma::vec appreciate(arma::vec vector,
                     float self_discharge, 
                     List eff, 
                     bool backwards){
  
  bool depreciate = false;
  return interest(vector = vector,
           self_discharge = self_discharge,
           eff = eff,
           backwards = backwards,
           depreciate = depreciate);
}

// [[Rcpp::export]]
arma::vec depreciate(arma::vec vector,
                     float self_discharge, 
                     List eff, 
                     bool backwards){
  
  bool depreciate = true;
  return interest(vector = vector,
           self_discharge = self_discharge,
           eff = eff,
           backwards = backwards,
           depreciate = depreciate);
}

// [[Rcpp::export]]
arma::vec cont_to_fct(arma::vec vector, 
                    int n_pieces){
  double rng = range(vector);
  double piece_size = rng/n_pieces;
  vector = vector - vector.min();
  
  arma::vec sol = arma::zeros<arma::vec>(vector.n_elem);
  
  for (int i=0; i < (n_pieces); ++i) {
    sol.elem(find(vector >= piece_size*i)).fill(i);  
  }
  return(sol);
}

// [[Rcpp::export]]
List backshiftCpp(arma::vec consumption, 
                  float self_discharge, 
                  List eff,
                  int horizon){
  
  // size of the piece
  int precision = 200;
  float piece = (max(consumption)-min(consumption))/precision;
  
  arma::vec vec_local_empty = arma::zeros<arma::vec>(horizon);
  arma::vec vec_local_cons = arma::zeros<arma::vec>(horizon);
  arma::vec vec_local_diff = arma::zeros<arma::vec>(horizon);
  arma::vec vec_offset = arma::zeros<arma::vec>(horizon);
  vec_offset.fill(NA_REAL);
  
  int cons_length = consumption.n_elem;
  // Define the size of the matrix
  arma::mat mtx_moves = arma::zeros<arma::mat>(cons_length*precision,4);
  // counter for the nrows of the matrix
  int nrow_moves = 0;
  
  for (int i=0; i < cons_length; ++i) {
    // copy of consumption with a bunch of NA before
    arma::vec cons_copy = vec_offset;
    cons_copy.insert_rows(cons_copy.n_rows, consumption);
    // `io` is `i` plus the offset of NAs that are at the start of cons_copy
    // To work with the time horizon that is before the initial time boundary. 
    int io = i + horizon;
    
    while(cons_copy[io] > 0) {
      
      arma::vec vec_local_deprec = depreciate(
        vec_local_empty.fill(cons_copy[io]),
        self_discharge, 
        eff, 
        true
      );
      // local consumption to compare with
      vec_local_cons = cons_copy.subvec(io - horizon + 1, io);
      // find the best point comparing the initial consumption
      // and the depreciation curve 
      vec_local_diff = vec_local_deprec - vec_local_cons;
      int loc_min = vec_local_diff.index_max();
      // if there is no good place to allocate, break
      if (vec_local_diff[loc_min] <= 0) break;
      // find the position to put the piece in 
      int pos_min = io - horizon + 1 + loc_min;
      double gain = (piece * cons_copy[io]) - (piece * cons_copy[pos_min]);
      // diminish the consumption and repeat
      cons_copy[io] = cons_copy[io] - piece;
      // populate the matrix: from
      mtx_moves(nrow_moves,0) = i;
      // times back
      mtx_moves(nrow_moves,1) = loc_min - horizon;
      // gain
      mtx_moves(nrow_moves,2) = gain;
      // additional row in the mtx_moves
      nrow_moves = nrow_moves + 1;
    }
  }
  // return a matrix with the right size
  mtx_moves = mtx_moves.head_rows(nrow_moves);
  
  // PART 2: RETURN THE MATRIX OF PRE-BACKSHIFT
  
  // define a vector of indices: 
  // what rows of mtx_moves provide more value
  arma::uvec mtx_moves_idx = arma::sort_index(mtx_moves.col(2), "descend");
  int index_length = mtx_moves_idx.n_elem;
  int gain_divisions = 5;
  mtx_moves.col(3) = cont_to_fct(mtx_moves.col(2),gain_divisions);

  arma::mat mtx_prebsh = arma::zeros<arma::mat>(cons_length,5);
  for (int row = 0; row < index_length; ++row) {
    int r = mtx_moves(row,0);
    int c = mtx_moves(row,3);
    mtx_prebsh(r, c) = mtx_prebsh(r, c) + piece;
  }
  // PART 3: DISTRIBUTE THE POINTS

  // to go faster, keep track if the point is not allowed anymore
  arma::vec vct_cons_allowed = arma::ones<arma::vec>(cons_length);
  // `cons_mutable` is defined outside the loop, because it changes
  // as the algorithm runs
  arma::vec cons_mutable = vec_offset;
  cons_mutable.insert_rows(cons_mutable.n_rows, consumption);
  // define the post-backshift matrix, as an empty copy of the previous one
  arma::mat mtx_postbsh = mtx_prebsh;
  mtx_postbsh.zeros();
  
  for (int e = 0; e < index_length; ++e) {
    // what row of mtx_moves are we talking about?
    // the one defined by the index in the position `e`
    int i = mtx_moves(mtx_moves_idx(e),0);
    // if the point is disabled, move on to the next
    // if (vct_cons_allowed[i] == 0) continue;
    // define `c`, the column the point belongs to
    int c = mtx_moves(mtx_moves_idx(e),3);
    // variable `io`, that includes the 
    int io = i + horizon;
 
    
    while(cons_mutable[io] > 0) {
      
      arma::vec vec_local_deprec = depreciate(
        vec_local_empty.fill(cons_mutable[io]),
        self_discharge, 
        eff, 
        true
      );
      // local consumption to compare with
      vec_local_cons = cons_mutable.subvec(io - horizon + 1, io);
      // find the best point comparing the initial consumption
      // and the depreciation curve 
      vec_local_diff = vec_local_deprec - vec_local_cons;
      int loc_min = vec_local_diff.index_max();
      // when no more gain in point, disallow it
      if (vec_local_diff[loc_min] <= 0) {
        // vct_cons_allowed[i] = 0;
        break;
      }
      // update the clone of consumption
      cons_mutable[io] = cons_mutable[io] - piece;
      
      int pos_min = io - horizon + loc_min + 1;
      cons_mutable[pos_min] = cons_mutable[pos_min] + piece;
      
      // update the postbsh matrix
      mtx_postbsh(pos_min - horizon,c) = mtx_postbsh(pos_min - horizon,c) + piece;
      
    }
  }

  arma::vec final_consumption = cons_mutable.tail(cons_length);
  
  return List::create(
    _["mtx_moves"]= mtx_moves,
                      // _["mtx_moves_idx"]= mtx_moves_idx,
                      _["mtx_prebsh"]= mtx_prebsh,
                      _["mtx_postbsh"]= mtx_postbsh,
                      _["final_consumption"]= final_consumption
                      // _["indices"]= indices
                        );
}
















