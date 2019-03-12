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
                  DataFrame params_df,
                  int horizon,
                  Environment env_fit,
                  Language call_fit,
                  Environment env_aux,
                  Language call_aux){
  
  // Start with the vectors to extract data frame parameters
  CharacterVector p_name = params_df["name"];
  NumericVector p_vol = params_df["vol"];
  NumericVector p_soc = params_df["soc"];
  NumericVector p_eff_to = params_df["eff_to"];
  NumericVector p_eff_from = params_df["eff_from"];
  NumericVector p_self_discharge = params_df["self_discharge"];
  NumericVector p_cap_to = params_df["cap_to"];
  NumericVector p_cap_from = params_df["cap_from"];
  
  // PROVISIONALLY, assign them to the first storage
  double self_discharge = p_self_discharge[0];
  List eff = List::create(p_eff_to[0], p_eff_from[0]);
  double vol = p_vol[0];
  
  // to reverse at the end the NA padding
  int init_length = consumption.n_elem;
  
  // every element in `env_fit` is extended with a NA of the horizon
  CharacterVector fname = as<CharacterVector>(env_fit.ls(true));
  for(int i = 0; i < fname.length(); ++i) {
    std::string thename = as<std::string>(fname[i]);
    env_fit[thename] = naPad(env_fit[thename], horizon);
  }
  //the same with the initial consumption
  consumption = naPad(consumption, horizon);
  
  // the length, once padded
  int padded_length = consumption.n_elem;
  
  // initialize vectors
  arma::vec v_soc = arma::zeros<arma::vec>(padded_length);

  // size of the piece
  int precision = 200;
  float piece = (max(consumption)-min(consumption))/precision;
  
  // define initial curve
  arma::vec fit_curve_initial = as<arma::vec>(Rf_eval(call_fit, env_fit));
  
  // define some hel vairables
  arma::vec vec_local_empty = arma::zeros<arma::vec>(horizon);
  arma::vec vec_local_fit = arma::zeros<arma::vec>(horizon);
  arma::vec vec_local_diff = arma::zeros<arma::vec>(horizon);
  
  
  arma::vec cons_copy_raw = env_fit[".demand"];
  
  
  // Define a matrix big enough to host moves
  arma::mat mtx_moves = arma::zeros<arma::mat>(init_length*precision,4);
  int nrow_mtx_moves = 0;
  
  arma::vec cons_master = env_fit[".demand"];
  
  // start in the `horizon`. Before, there are only NAs
  for (int i = horizon; i < padded_length; ++i) {
    // grab a copy of consumption padded
    
    arma::vec cons_copy = cons_master;
    
    while(cons_copy[i] > piece) {
      
      env_fit[".demand"] = cons_copy;
      arma::vec ifit = as<arma::vec>(Rf_eval(call_fit, env_fit));
      
      arma::vec vec_local_deprec = depreciate(
        vec_local_empty.fill(ifit[i]),
        self_discharge, 
        eff, 
        true
      );
   
      // local fitting to compare with
      // INSTEAD OF fit_curve_initial
      vec_local_fit = ifit.subvec(i - horizon, i - 1);
      // find the best point comparing the initial consumption
    
      vec_local_diff = vec_local_deprec - vec_local_fit;
      int loc_min = vec_local_diff.index_max();
      // if there is no good place to allocate, break
      if (vec_local_diff[loc_min] <= 0) break;
      // find the position to put the piece in 
      int pos_min = i - horizon + loc_min;
      // the gain has directly to do with the initial fitting curve
      //       // INSTEAD OF fit_curve_initial
      double gain = (piece * ifit[i]) - (piece * (ifit[pos_min] + piece));
      if (gain <= 0) break;
      // to check if gain is NA. Alternative??
      if (gain*0 != 0) break;
      // diminish the consumption and repeat
      cons_copy[i] -= piece;
      // populate the matrix: from
      mtx_moves(nrow_mtx_moves,0) = i;
      // times back (not useful anymore?)
      //mtx_moves(nrow_mtx_moves,1) = loc_min - horizon;
      // gain
      mtx_moves(nrow_mtx_moves,2) = gain;
      
      // update row counter
      nrow_mtx_moves = nrow_mtx_moves + 1;
    }
  }
  // return a matrix with the right size
  mtx_moves = mtx_moves.head_rows(nrow_mtx_moves);
  
  // PART 2: RETURN THE MATRIX OF PRE-BACKSHIFT

  // define a vector of indices: 

  // what rows of mtx_moves provide more value
  arma::uvec mtx_moves_idx = arma::sort_index(mtx_moves.col(2), "descend");
  int index_length = mtx_moves_idx.n_elem;
  int gain_divisions = 5;
  mtx_moves.col(3) = contToFct(mtx_moves.col(2),gain_divisions);
  
  // potential to allocate bashshift (pre)
  arma::mat mtx_prebsh = arma::zeros<arma::mat>(padded_length,gain_divisions);
  for (int row = 0; row < index_length; ++row) {
    int r = mtx_moves(row,0);
    int c = mtx_moves(row,3);
    mtx_prebsh(r, c) = mtx_prebsh(r, c) + piece;
  }
  // PART 3: DISTRIBUTE THE POINTS
 
  // reboot the consumption of env_fit
  env_fit[".demand"] = cons_master;
  
  // define the post-backshift matrix, as an empty copy of the previous one
  arma::mat mtx_postbsh = arma::zeros<arma::mat>(padded_length,5);
  
  for (int e = 0; e < index_length; ++e) {
    
    arma::vec cons_mutable = env_fit[".demand"];
    
    // what row of mtx_moves are we talking about?
    // the one defined by the index in the position `e`
    int i = mtx_moves(mtx_moves_idx(e),0);
    // if the point is disabled, move on to the next
    // if (vct_cons_allowed[i] == 0) continue;
    // define `c`, the column the point belongs to
    int c = mtx_moves(mtx_moves_idx(e),3);
   
  
    arma::vec ifit = as<arma::vec>(Rf_eval(call_fit, env_fit));
    
    ifit = ifit.subvec(i - horizon, i - 1); 
    
    arma::vec vec_local_deprec = depreciate(
      vec_local_empty.fill(ifit[horizon - 1]), //horizon
      self_discharge, 
      eff, 
      true
    );
   
    // find the best point to allocate
    vec_local_diff = vec_local_deprec - ifit;
    int loc_min = vec_local_diff.index_max();
    int pos_min = i - horizon + loc_min;
    double gain = (piece * ifit[horizon - 1]) - (piece * (ifit[loc_min]));
    // no gain, just continue
    if (gain <= 0) {
      continue;
    } 
    
    // update the clone of consumption ...
    cons_mutable[i] -= piece; 
    cons_mutable[pos_min] += piece; 
    // ... the v_soc vector ...
    v_soc[i] -= piece; 
    v_soc[pos_min] += piece;
    
    // ... and the environment, for evaluation
    env_fit[".demand"] = cons_mutable;
    
    // update the postbsh matrix
    mtx_postbsh(pos_min, c) += piece; 
  }

  arma::vec final_consumption = env_fit[".demand"];
  
  arma::vec fit_curve_final = as<arma::vec>(Rf_eval(call_fit, env_fit));
  
  // remove the NAs from the start
  mtx_prebsh = mtx_prebsh.tail_rows(init_length);
  mtx_postbsh = mtx_postbsh.tail_rows(init_length);
  final_consumption = final_consumption.tail(init_length);
  fit_curve_initial = fit_curve_initial.tail(init_length);
  fit_curve_final = fit_curve_final.tail(init_length);
  
  v_soc = v_soc.tail(init_length);
    
  return List::create(
    _["mtx_moves"]= mtx_moves,
    // _["mtx_moves_idx"]= mtx_moves_idx,
    _["mtx_prebsh"]= mtx_prebsh,
    _["mtx_postbsh"]= mtx_postbsh,
    _["final_consumption"]= final_consumption,
    _["fit_curve_initial"]= fit_curve_initial,
    _["fit_curve_final"] = fit_curve_final,
    _["v_soc"] = v_soc
    );
}
















