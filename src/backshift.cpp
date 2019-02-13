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
arma::vec contToFct(arma::vec vector, 
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
arma::vec naPad(arma::vec vector, 
                int horizon){
  
  arma::vec vec_offset = arma::zeros<arma::vec>(horizon);
  vec_offset.fill(NA_REAL);
  vec_offset.insert_rows(vec_offset.n_rows, vector);
  return vec_offset;
  
}

// [[Rcpp::export]]
Environment naPadEnv(Environment input,
                       Environment out,
                       int horizon) {
  // returns an environment with all variables
  // padded with NA at the start
  
  CharacterVector fname = as<CharacterVector>(input.ls(true));
  int n = fname.length();
  
  for(int i = 0; i < n; ++i) {
    std::string thename = as<std::string>(fname[i]);
    out[thename] = naPad(input[thename], horizon);
  }
  return out;
}

// [[Rcpp::export]]
arma::vec sliceCurrent2(arma::vec vec,
                            int start,
                            int end) {
  int n = vec.n_elem;
  if ((start + end) >= n) end = 0; //1+ ?
  return vec.subvec(start , start + end);
}

// [[Rcpp::export]]
Environment envCurrent2(Environment input,
                        Environment out,
                        int start,
                        int span) {
  // returns an environment output with all variables
  // cut by present.
  CharacterVector fname = as<CharacterVector>(input.ls(true));
  int n = fname.length();
  
  for(int i = 0; i < n; ++i) {
    std::string thename = as<std::string>(fname[i]);
    out[thename] = sliceCurrent2(input[thename], start, span);
  }
  return out;
}

// [[Rcpp::export]]
List backshiftCpp(arma::vec consumption, 
                  float self_discharge, 
                  List eff,
                  int horizon,
                  Environment env_fit,
                  Language call_fit,
                  Environment env_aux,
                  Language call_aux){
  
  horizon = horizon; //+1?

  // every element in `env_fit` is extended with a NA of the horizon ??
  env_fit = naPadEnv(env_fit, env_fit, horizon);

  // size of the piece
  int precision = 200;
  float piece = (max(consumption)-min(consumption))/precision;
  
  // define initial curve
  arma::vec fit_curve_initial = as<arma::vec>(Rf_eval(call_fit, env_fit));
  
  arma::vec vec_local_empty = arma::zeros<arma::vec>(horizon);
  arma::vec vec_local_fit = arma::zeros<arma::vec>(horizon);
  arma::vec vec_local_diff = arma::zeros<arma::vec>(horizon);
  
  
  arma::vec cons_copy_raw = env_fit[".demand"];
  
  int cons_length = consumption.n_elem;
  // Define the size of the matrix
  arma::mat mtx_moves = arma::zeros<arma::mat>(cons_length*precision,4);
  // counter for the nrows of the matrix
  int nrow_moves = 0;
  
  arma::vec cons_master = env_fit[".demand"];
  
  
  for (int i=0; i < cons_length; ++i) {
    // grab a copy of consumption padded
   
    //// cons_copy.insert_rows(cons_copy.n_rows, consumption);
    // `io` is `i` plus the offset of NAs that are at the start of cons_copy
    // To work with the padded vectors
    int io = i + horizon;
    
    arma::vec cons_copy = cons_master;
    // arma::vec cons_mod = env_fit[".demand"];
    // arma::vec fit_curve_local = as<arma::vec>(Rf_eval(call_fit, env_fit));
    
    while(cons_copy[io] > piece) {
      
      env_fit[".demand"] = cons_copy;
      arma::vec fit_curve_local = as<arma::vec>(Rf_eval(call_fit, env_fit));
      
      arma::vec vec_local_deprec = depreciate(
        vec_local_empty.fill(fit_curve_local[io]),
        self_discharge, 
        eff, 
        true
      );
   
      // local fitting to compare with
      // INSTEAD OF fit_curve_initial
      vec_local_fit = fit_curve_local.subvec(io - horizon, io - 1); //??
      // find the best point comparing the initial consumption
    
      vec_local_diff = vec_local_deprec - vec_local_fit;
      int loc_min = vec_local_diff.index_max();
      // if there is no good place to allocate, break
      if (vec_local_diff[loc_min] <= 0) break;
      // find the position to put the piece in 
      int pos_min = io - horizon + loc_min;
      // the gain has directly to do with the initial fitting curve
      //       // INSTEAD OF fit_curve_initial
      double gain = (piece * fit_curve_local[io]) - (piece * (fit_curve_local[pos_min]+piece));
      if (gain <= 0) break;
      // to check if gain is NA. Alternative??
      if (gain*0 != 0) break;
      // diminish the consumption and repeat
      // OR REDO THE CURVE?
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
  mtx_moves.col(3) = contToFct(mtx_moves.col(2),gain_divisions);

  arma::mat mtx_prebsh = arma::zeros<arma::mat>(cons_length,5);
  for (int row = 0; row < index_length; ++row) {
    int r = mtx_moves(row,0);
    int c = mtx_moves(row,3);
    mtx_prebsh(r, c) = mtx_prebsh(r, c) + piece;
  }
  // PART 3: DISTRIBUTE THE POINTS
 
 // reboot the consumption of env_fit
 env_fit[".demand"] = cons_master;
  
  // cons_mutable.insert_rows(cons_mutable.n_rows, consumption);
  
  // define the post-backshift matrix, as an empty copy of the previous one
  arma::mat mtx_postbsh = arma::zeros<arma::mat>(cons_length,5);
  
  for (int e = 0; e < index_length; ++e) {
    
    arma::vec cons_mutable = env_fit[".demand"];
    
    // what row of mtx_moves are we talking about?
    // the one defined by the index in the position `e`
    int i = mtx_moves(mtx_moves_idx(e),0);
    // if the point is disabled, move on to the next
    // if (vct_cons_allowed[i] == 0) continue;
    // define `c`, the column the point belongs to
    int c = mtx_moves(mtx_moves_idx(e),3);
    // variable `io`, that includes the 
    int io = i + horizon;
    
    // env_aux = envCurrent2(env_fit, env_aux, i, horizon - 1);
    // local fit to compare with
    // arma::vec ifit = as<arma::vec>(Rf_eval(call_fit, env_aux));
    arma::vec ifit = as<arma::vec>(Rf_eval(call_fit, env_fit));
    
    ifit = ifit.subvec(io - horizon, io - 1); 
    
    arma::vec vec_local_deprec = depreciate(
      vec_local_empty.fill(ifit[horizon - 1]), //horizon
      self_discharge, 
      eff, 
      true
    );
    
    if (e == 20) {
      Rcout << " ifit: ";
      Rcout << ifit;
      Rcout << " ifit[horizon]: ";
      Rcout << ifit[horizon - 1];
      Rcout << " vec_local_deprec: ";
      Rcout << vec_local_deprec;
    }
    
    // vec_local_cons = as<arma::vec>(Rf_eval(call_fit, env_fit));
    // find the best point comparing the initial consumption
    // and the depreciation curve 
    
    vec_local_diff = vec_local_deprec - ifit;
    
    
    
    
    int loc_min = vec_local_diff.index_max();
    // when no more gain in point, disallow it
    // if (vec_local_diff[loc_min] <= 0) {
    //   // vct_cons_allowed[i] = 0;
    //   break;
    // }
   
    int pos_min = io - horizon + loc_min;
    double gain = (piece * ifit[horizon - 1]) - (piece * (ifit[loc_min]));
    
    
    if (gain <= 0) {
      continue;
    } 
    
    // update the clone of consumption
 
    cons_mutable[io] = cons_mutable[io] - piece;
    cons_mutable[pos_min] = cons_mutable[pos_min] + piece;
    env_fit[".demand"] = cons_mutable;
    
    // update the postbsh matrix

    mtx_postbsh(pos_min - horizon,c) = mtx_postbsh(pos_min - horizon,c) + piece;
  }

  arma::vec final_consumption = env_fit[".demand"];
  final_consumption = final_consumption.tail(cons_length);
  
  arma::vec fit_curve_final = as<arma::vec>(Rf_eval(call_fit, env_fit));
  
  fit_curve_initial = fit_curve_initial.tail(cons_length);
  fit_curve_final = fit_curve_final.tail(cons_length);
    
  return List::create(
    _["mtx_moves"]= mtx_moves,
                      // _["mtx_moves_idx"]= mtx_moves_idx,
                      _["mtx_prebsh"]= mtx_prebsh,
                      _["mtx_postbsh"]= mtx_postbsh,
                      _["final_consumption"]= final_consumption,
                      _["fit_curve_initial"]= fit_curve_initial,
                      _["fit_curve_final"] = fit_curve_final
                        );
}
















