#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
// [[Rcpp::plugins("cpp11")]]


// [[Rcpp::export]]
NumericMatrix formatFlexSteps(NumericMatrix matrix,
                              IntegerVector flex_step,
                              int max_step = -1) {

  IntegerVector flex_lvl = seq(1, max(flex_step));

  if (max_step != -1) {
    flex_lvl = seq(1, max_step);
  }

  // Empty matrix with the right dimensions
  NumericMatrix mtx(matrix.nrow(), flex_lvl.size());

  // Allocate the columns to its right place in mtx, using flex_step as index
  for (int i=0; i < flex_step.size(); ++i) {
    mtx(_ , flex_step[i]-1) = matrix(_ , i);
  }

  return mtx;
}


// TO CALL INTERNALLY

// [[Rcpp::export]]
NumericVector divideInChunks(float x, float precision){
  if (x == 0) return 0;
  int full = floor(x/precision);
  float left = fmod(x, precision);
  NumericVector v = rep(precision, full);
  v.push_back(left);
  return v;
}


// [[Rcpp::export]]
int whichMin (NumericVector x){
  int res = 0;
  LogicalVector finite = is_finite(x);
  for (int i=0; i < x.size(); ++i) {
    if ((finite[i] == 1) & ((x[i] < x[res]) | (finite[res] == 0))) res = i;
  }
  return res;
}



// [[Rcpp::export]]
NumericVector sliceCurrent (NumericVector vec,
                            int start,
                            int end) {
  
  int true_end = end + 1;
  if (start + end >= vec.size()) true_end = 0; //1+ ?
  return vec.import(vec.begin()+start , vec.begin() + start + true_end);
}

// [[Rcpp::export]]
Environment envCurrent(Environment input,
                       Environment out,
                       int start,
                       int span) {
  // returns an environment output with all variables
  // cut by present.
  CharacterVector fname = as<CharacterVector>(input.ls(true));
  int n = fname.length();
  
  for(int i = 0; i < n; ++i) {
    std::string thename = as<std::string>(fname[i]);
    out[thename] = sliceCurrent(input[thename], start, span);
  }
  return out;
}

// [[Rcpp:: export]]
arma::mat asMat (NumericMatrix x) {
  arma::mat y = as<arma::mat>(x) ;
  return y;
}


// [[Rcpp:: export]]
NumericMatrix asNumericMatrix (arma::mat x) {
  NumericMatrix y = wrap(x) ;
  return y ;
}


// [[Rcpp::export]]
arma::cube listToCube (List mtx_list){
  
  NumericMatrix zmtx = mtx_list[0];
  arma::cube ncube(zmtx.nrow(),zmtx.ncol(),mtx_list.size());
  ncube.zeros();
  
  for (int i=0; i < mtx_list.size(); ++i) {
    ncube.slice(i) = asMat(mtx_list[i]);
  }
  return ncube;
}

// [[Rcpp::export]]
List cubeToList (arma::cube xcube){
  
  List nlist = List::create();
  int n = xcube.n_slices;
  
  for (int i=0; i < n; ++i) {
    nlist.push_back(xcube.slice(i));
  }
  return nlist;
}


// [[Rcpp::export]]
List foreShiftCpp(List mtx_list,
                  NumericVector cap,
                  Environment env_fit,
                  Language call_fit,
                  Environment env_aux,
                  Language call_aux
){
  
  // define initial curve
  NumericVector fit_curve_initial = Rf_eval(call_fit, env_fit);
  
  // define initial demand from the argument passed from above
  NumericVector cdemand = Rf_eval(call_aux, env_fit);
  NumericVector zdemand = clone(cdemand);
  // and an empty vector for the flexible demand
  // NumericVector fdemand = clone(cdemand);
  // fdemand.fill(0);
  // and an empty vector for the flexible demand of each object
  NumericVector odemand = clone(cdemand);
  odemand.fill(0);
  
  // build a cube with the input
  arma::cube xcube = listToCube(mtx_list);
  
  // define an empty cube for the results
  arma::cube fcube = xcube;
  fcube.zeros();
  // Chunk size for object and column (mean/30)
  arma::mat mtx_cmean = mean(xcube, 0)/30;
  
  // if same column and row, the objects distribute the chunks proportionally
  arma::mat mtx_cssumn = sum(xcube, 2);
  arma::cube pcube = xcube.each_slice() / mtx_cssumn;
  
  // the algorithm
  int n_col = xcube.n_cols; // flexibility
  int n_row = xcube.n_rows; // time
  int n_slice = xcube.n_slices; // object

  for (int c=1; c < n_col; ++c) {
    for (int s=0; s < n_slice ; ++s) {
      
      
      //reboot the object demand
      odemand.fill(0);
      
      //find the cap for this object
      double icap = cap[s];
      
      
      
      for (int r=0; r < n_row; ++r) {
        
        // if there is nothing to distribute, go directly to the next iteration
        if (xcube(r, c, s) == 0) continue;

        // at the last rows(time), the flexible is not considered
        if ( r+1 >= n_row - c) break;
        
        // divide to distribute in chunks of the indicated size
        double m = 0;
        
        //selecting the mean is different if there is only one slice
        if (n_slice == 1) m = mtx_cmean(s,c); else  m = mtx_cmean(c,s);
        NumericVector chunks = divideInChunks(xcube(r,c,s), m);
        
        for (int i=0; i < chunks.size(); ++i) {
          
          // Recalculate the local fit curve in its environment
          env_aux = envCurrent(env_fit, env_aux, r, c);
          NumericVector ifit = Rf_eval(call_fit, env_aux);
          
          
          
          // works with cap
          if (icap > 0) {
            // check current flexibility allocated
            NumericVector iflex = sliceCurrent(odemand,r,c);
            
            // Rcout << iflex;
            
            // disallow (convert to NA) the values higher than the cap of the object
            if (is_true(any(iflex < icap))) {
              for (int k=0; k < iflex.size(); ++k) {
                if (iflex[k] >= icap) ifit[k] = NA_REAL;
              }
            }

            // DEPLOY THE WATCH
            
            // slide to the future if it is not possible to allocate any chunk
            // if (is_true(all(iflex >= cap)) & (cap_spread == true)) {
            //   xcube(r+1,c,s) = xcube(i+1,c,s) +
            //     sum(chunks.import(chunks.begin()+i, chunks.end()));
            //   // update the proportion cube
            //   arma::mat mtx_cssumn = sum(xcube, 2);
            //   arma::cube pcube = xcube.each_slice() / mtx_cssumn;
            //   break;
            // }
          }

          // Where to put the chunk
          int imin = whichMin(ifit);
          
          // the chunk may be distributed over several objects in fcube
          fcube.tube(r+imin, c) = fcube.tube(r+imin, c) + (chunks[i] * pcube.tube(r,c));
          //fcube(r+imin,c,s) = fcube(r+imin,c,s) + chunks[i]; // the previous alternative

          // and to the total of flex consumption
          cdemand(r+imin) = cdemand(r+imin) + chunks[i];
          // fdemand(r+imin) = fdemand(r+imin) + chunks[i];
          odemand(r+imin) = odemand(r+imin) + chunks[i];
          // the demand in env_fit is updated with the just allocated one
          env_fit[".demand"] = cdemand;
        }
      }
    }
  }

  //calculate the final curve
  NumericVector fit_curve_final = Rf_eval(call_fit, env_fit);
  
  // unstack fcube in a list
  List flist = cubeToList(fcube);
  
  List sol = List::create(_["demand_fixed"]= zdemand,
                          _["demand_flex"]= flist,
                          _["fit_curve_initial"]= fit_curve_initial,
                          _["fit_curve_final"] = fit_curve_final);
  
  return sol;
}


