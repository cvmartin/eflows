#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector divide(float x, float precision = 0.01){
  if (x == 0) return 0;
  int full = floor(x/precision);
  float left = fmod(x, precision);
  NumericVector v = rep(precision, full);
  v.push_back(left);
  return v;
}

// [[Rcpp::export]]
NumericVector present (NumericVector vec,
                       int start,
                       int end) {

  int true_end = end + 1;
  if (1 + start + end >= vec.size()) true_end = 0;
  return vec.import(vec.begin()+start , vec.begin() + start + true_end);
}


// [[Rcpp::export]]
DatetimeVector xts_index(NumericMatrix xts_mtx) {
  if (Rf_isNull(xts_mtx.attr("index"))) return 0;
  DatetimeVector index(NumericVector(xts_mtx.attr("index")));
  return index;
}


// [[Rcpp::export]]
double signif_ccp (double value, int digits) {
  if (value == 0.0) // otherwise it will return 'nan' due to the log10() of zero
    return 0.0;

  double factor = pow(10.0, digits - ceil(log10(fabs(value))));
  return round(value * factor) / factor;
}
// [[Rcpp::export]]
int tell_min (NumericVector x){
  int res = 0;
  LogicalVector finite = is_finite(x);
  for (int i=0; i < x.size(); ++i) {
    if ((finite[i] == 1) & ((x[i] < x[res]) | (finite[res] == 0))) res = i;
  }
  return res;
}


// [[Rcpp::export]]
LogicalVector tellmin2 (NumericVector x){
  return is_finite(x);
}

// [[Rcpp::export]]
NumericVector signif_step (NumericMatrix matrix,
                           float den = 10) {

  int n = matrix.ncol();
  NumericVector m_means(n);
  for (int i=0; i < n; ++i) {
    m_means[i] = mean(matrix( _, i));
  }

  NumericVector m_steps(n);
  for (int i=0; i < n; ++i) {
    m_steps[i] = signif_ccp(m_means[i],3)/den;
  }

  return m_steps;
}

