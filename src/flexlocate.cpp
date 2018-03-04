#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector divide(float x, float precision = 0.01){
  int full = floor(x/precision);
  float left = fmod(x, precision);
  NumericVector v = rep(precision, full);
  v.push_back(left);
  return v;
}


// [[Rcpp::export]]
NumericVector sumvect(NumericVector x,
                      NumericVector y){
  // Dude, you don't need this, just sum up
  NumericVector res = x;
  for (int i=0; i < res.size(); ++i) {
    res[i] = res[i] + y[i];
  }
  return res;
}
// When you assign a object v1 to another object v2 using = operator (v2 = v1;), the value of elements of v1 is not copied to v2 but v2 will be an alias to v1. Thus, if you change the value of some elements in v1, the change also applied to v2. You should use clone(), if you want to avoid coupling between objects (see sample code below).


// [[Rcpp::export]]
NumericMatrix flexlocate(NumericMatrix matrix,
                         IntegerVector steps){

  IntegerVector xsteps = seq(0, max(steps));

  NumericMatrix mx(matrix.nrow(), xsteps.size());
  NumericMatrix fx = mx;

  for (int i=0; i < steps.size(); ++i) {
    mx(_ , steps[i]) = matrix(_ , i);
  }

  // Define solid consumption and total consumption
  // Just a couple of vectors that can be used for fit
  NumericVector solid = mx( _ ,0);
  NumericVector total(matrix.nrow());
  for (int i=0; i < matrix.ncol(); ++i) {
    total = sumvect(total, matrix(_, i));
  }
  // Define the fitting curve. start simple:
  NumericVector fit = solid;

// Loop over columns, then over rows
    for (int h=1; h < matrix.ncol(); ++h) {
      for (int i=0; i < matrix.nrow(); ++i) {

        NumericVector pieces = divide(mx(i,h));

        // Ensure that it doesn't accumulate at the end
        if((mx.nrow() - i) < h) pieces(0);

        NumericVector xfit(fit.begin()+(i), fit.begin()+(i+h));
        int min = which_min(xfit);

        for (int j=0; j < pieces.size(); ++j){
          fx((i + min), h) = fx((i + min), h) + pieces[j];
          //  Recalculate
          fit((i + min)) = fit((i + min)) + pieces[j];
          xfit[min] = xfit[min] + pieces[j];
          int min = which_min(xfit);
        }
      }
    }
  return fx;
}
