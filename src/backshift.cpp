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
  
  // how to determine the nrow of the matrix?
  arma::mat mtx_moves = arma::zeros<arma::mat>(10000,3);
  // counter for the nrows of the matrix
  int r = 0;
  
  int loc_min = 0;
  int cons_length = consumption.n_elem;
  double gain = 0;
  double piece = (max(consumption)-min(consumption))/100;
  
  arma::vec vec_local_empty = arma::zeros<arma::vec>(horizon);
  arma::vec vec_local_cons = arma::zeros<arma::vec>(horizon);
  arma::vec vec_local_diff = arma::zeros<arma::vec>(horizon);
  
  arma::vec vec_offset = arma::zeros<arma::vec>(horizon);
  vec_offset.fill(NA_INTEGER);
  
  for (int i=0; i < cons_length; ++i) {
    // start with an afresh copy of consumption
    arma::vec cons_copy = consumption;
    // arma::vec cons_copy = vec_offset;
    // cons_copy.insert_rows(cons_copy.n_rows, consumption);
    
    // if (i == 23) {
    //   Rcout << cons_copy;
    // }
    // 
    while(cons_copy[i] > 0) {
      
      arma::vec vec_local_deprec = depreciate(
        vec_local_empty.fill(cons_copy[i]),
        self_discharge, 
        eff, 
        true
      );
      // if I wanted to consider only one piece...
      // deprec = deprec + cons_copy[i] - (piece);
     
     int start_from = i-horizon + 1;
     if (start_from < 0) break; //start_from = 0;
      
      vec_local_cons = cons_copy.subvec(start_from, i);
      
      // if (start_from == 0) {
      //   vec_local_deprec = vec_local_deprec.tail(vec_local_cons.n_elem);
      // }
      
      // Rcout << "vec_local_deprec: ";
      // Rcout << vec_local_deprec;
      // Rcout << "vec_local_cons: ";
      // Rcout << vec_local_cons;
      
      vec_local_diff = vec_local_deprec - vec_local_cons;
      
      
      loc_min = vec_local_diff.index_max();
      
      // if (i == position){
      //   Rcout << "deprec";
      //   Rcout << deprec;
      //   Rcout << "\n";
      //   Rcout << "vec_local_diff";
      //   Rcout << vec_local_diff;
      //   veccon = vec_local_cons;
      //   vecdep = deprec;
      //   Rcout << "loc_min: ";
      //   Rcout << loc_min;
      // }
      
      // if there is no good place to allocate, break
      if (vec_local_diff[loc_min] <= 0) break;
      
      cons_copy[i] = cons_copy[i] - piece;
      
      int pos_min = i - horizon + loc_min;
        
      // if (start_from == 0) {
      //   pos_min = loc_min;
      // }
      
      gain = (piece * cons_copy[i]) - (piece * cons_copy[pos_min]);
      
      // populate the matrix: from
      mtx_moves(r,0) = i;
      // to
      mtx_moves(r,1) = i - horizon + loc_min;
      // gain
      mtx_moves(r,2) = gain;
      
      r = r+1;
    
    }
  }
  
  mtx_moves = mtx_moves.head_rows(r);
  
  return List::create(_["mtx_moves"]= mtx_moves);
}
















