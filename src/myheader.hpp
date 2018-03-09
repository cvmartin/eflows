#include <Rcpp.h>
using namespace Rcpp;
NumericVector divide(float x, float precision);
NumericVector present (NumericVector vec, int start, int end);
DatetimeVector xts_index(NumericMatrix xts_mtx);
double signif_ccp (double value, int digits);
int tell_min (NumericVector x);
NumericVector signif_step (NumericMatrix matrix, float denom = 10);
