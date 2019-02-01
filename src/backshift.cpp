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
List backshiftCpp(arma::vec consumption, 
                  float self_discharge, 
                  List eff,
                  int horizon){
  
  // size of the piece
  int precision = 100;
  double piece = (max(consumption)-min(consumption))/precision;
  
  arma::vec vec_local_empty = arma::zeros<arma::vec>(horizon);
  arma::vec vec_local_cons = arma::zeros<arma::vec>(horizon);
  arma::vec vec_local_diff = arma::zeros<arma::vec>(horizon);
  arma::vec vec_offset = arma::zeros<arma::vec>(horizon);
  vec_offset.fill(NA_REAL);
  
  int cons_length = consumption.n_elem;
  // Define the size of the matrix
  arma::mat mtx_moves = arma::zeros<arma::mat>(cons_length*precision,3);
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
      // diminish the consumption and repeat
      cons_copy[io] = cons_copy[io] - piece;
      // find the position to put the piece in 
      int pos_min = io - horizon + loc_min;
      double gain = (piece * cons_copy[io]) - (piece * cons_copy[pos_min]);
      
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
  
  mtx_moves = mtx_moves.head_rows(nrow_moves);
  
  // PART 2: SETUP THE POINTS
  
  return List::create(_["mtx_moves"]= mtx_moves);
}
















