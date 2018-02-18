#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins("cpp11")]]



// [[Rcpp::export]]
NumericVector allocate (double flow,
                       NumericVector pref_v,
                       NumericVector soc_v,
                       NumericVector cap_v) {
  Function f("allocate");

  return f(flow, pref_v,soc_v, cap_v);
}


