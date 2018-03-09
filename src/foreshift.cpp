#include <Rcpp.h>
using namespace Rcpp;

//' Shift energy consumption towards the future.
//'
//' In function of a fitting curve and the flexibility time, the consumption of
//' energy is delayed.
//' @param matrix Numeric matrix, each row an observation in time (it can be a time series)
//' and each column representing the flexibility of demand, increasing towards the right.
//' @param flex_step Integer vector of same length of the column of `matrix`, indicating
//' the number of timesteps the demand can be delayed. Normally, the first integer is `0`,
//' indicating the `solid` demand.
//' @param cap If higher than 0, indicates the maximum capacity of flexible demand
//' that can be allocated in a timestep.
//' @param cap_spread Boolean. if true, the cap is never exceeded, and instead the
//' consumption is displaced towards the future, and resolved as soon as possible.
//' @param foresee Boolean. If false, it indicates a complete lack of knowledge of the
//' future flexible demand in the timeline.
//' @param solar If present, the fitting curve is calculated substracting solar
//' from the demand.
//'
//' @return A matrix, or an xts object if the index provided are a POSIXct object.
//' @export
// [[Rcpp::export]]
NumericMatrix foreshift(NumericMatrix matrix,
                        IntegerVector flex_step,
                        float cap = 0,
                        bool cap_spread = true,
                        bool foresee = true,
                        NumericVector solar = NumericVector::create(0)){

  // Define how many levels are there
  IntegerVector flex_lvl = seq(0, max(flex_step));

  // Two empty matrices (the second one has to be a deep copy of the first one)
  NumericMatrix mtx(matrix.nrow(), flex_lvl.size());
  NumericMatrix flex_mtx(clone(mtx));

  // Allocate the columns to its right place in mtx, using flex_step as index
  for (int i=0; i < flex_step.size(); ++i) {
    mtx(_ , flex_step[i]) = matrix(_ , i);
  }

  int n_row = mtx.nrow();
  int n_col = mtx.ncol();

  // Define solid consumption and total consumption
  NumericVector solid_vct = mtx( _ ,0);
  NumericVector flex_vct(n_row);

  // The first column of the flex matrix is the solid
  flex_mtx(_,0) = clone(solid_vct);

  // size of the chunks to split the columns in
  NumericVector chunks_size = signif_step(mtx);

  for (int e=1; e < n_col; ++e) {
    for (int i=0; i < n_row; ++i) {

      NumericVector chunks = divide(mtx(i,e), chunks_size[e]);

      // At the end of the graph, the flexible is not considered
      // int margin = e+1;
      if ( i+1 >= n_row - e) chunks = 0;

      for (int j=0; j < chunks.size(); ++j) {

        // Recalculate fitting curve
        NumericVector ifit = present(solid_vct,i,e) + present(flex_vct,i,e);
        // if applied solar, modify it
        if (solar.size() != 1){
          ifit = ifit - present(solar,i,e);
        }
        // Apply cap, if necessary
        NumericVector iflex = present(flex_vct,i,e);



        if (is_true(any(iflex < cap)) & (cap > 0)) {
          for (int k=0; k < iflex.size(); ++k) {
            if (iflex[k] >= cap) ifit[k] = NA_REAL;
          }
        }
        // Find the spot where to put the chunk
        int imin = tell_min(ifit);

        // slide forward if necessary
        if (is_true(all(iflex >= cap)) & (cap > 0) & (cap_spread == true)) {
          // if all is over the cap, send the leftover chunks to the future
          mtx(i+1,e) = mtx(i+1,e) + sum(chunks.import(chunks.begin()+j, chunks.end()));
          break;
        }
        // Add the chunk to the right place of the matrix,
        // and to the total of flex consumption
        flex_mtx(i+imin, e) = flex_mtx(i+imin, e) + chunks[j];
        flex_vct(i+imin) = flex_vct(i+imin) + chunks[j];
      }
    }

  }

  if (is<DatetimeVector>(xts_index(matrix))) {
    DatetimeVector time_index = xts_index(matrix);
    flex_mtx.attr("index") = time_index;
    flex_mtx.attr("class") = CharacterVector::create("xts", "zoo");
  }

  return flex_mtx;
}
