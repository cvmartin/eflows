#include <Rcpp.h>
#include"../inst/include/myheader.hpp"
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector subset_range (NumericVector vec,
                            int start,
                            int end) {

  int true_end = end+1; //
  // if (1 + start + end >= vec.size()) true_end = 0;
  return vec.import(vec.begin()+start , vec.begin() + start + true_end);
}

// // [[Rcpp::export]]
// DatetimeVector xts_index(NumericMatrix xts_mtx) {
//   if (Rf_isNull(xts_mtx.attr("index"))) return 0;
//   DatetimeVector index(NumericVector(xts_mtx.attr("index")));
//   return index;
// }

// [[Rcpp::export]]
float depreciate(float x,
                 float depreciation) {
  return  powf((1-depreciation), x);
}

// [[Rcpp::export]]
NumericVector seq_depreciated(int length,
                              float depreciation) {
  IntegerVector sequence = seq(1,length);
  NumericVector deprec(sequence.size());
  for (int i=0; i < deprec.size(); ++i) {
    deprec[i] = depreciate(sequence[i],depreciation);
  }
  return deprec;
}

//' Shift energy consumption towards the past.
//'
//'
//' @return A matrix, or an xts object if the index provided are a POSIXct object.
//' @export
// [[Rcpp::export]]
NumericMatrix backshift (NumericMatrix matrix,
                             NumericVector price,
                             int horizon,
                             float depreciation,
                             float cap,
                             float size) {

  NumericVector prod = matrix(_,0);
  NumericVector comp = matrix(_,1);
  NumericVector comp_init = clone(comp);
  NumericVector cost = comp * price;
  NumericVector cost_init = clone(cost);
  NumericVector balance(matrix.nrow());
  NumericVector storage(matrix.nrow());
  NumericVector to_battery(matrix.nrow());

  NumericVector deprec = seq_depreciated(horizon-1, depreciation);
  deprec.push_front(1);

  for (int i=1; i < (matrix.nrow()-horizon); ++i) {

    float avail = size/(1-depreciation) - storage[i-1];

    NumericVector icost = subset_range(cost, i, (horizon-1));
    NumericVector icost_real(horizon, icost[0]);
    icost_real = icost_real / deprec;

    while (is_true(any(icost > icost_real))) {
      int imax = which_max(icost - icost_real);

      float gain = 0.1 / deprec[imax];
      if (avail < gain) gain = avail;


      comp[i] = comp[i] + gain;
      to_battery[i] = to_battery[i] + gain;
      comp[(i+imax)] = comp[(i+imax)] - 0.1;

      avail = avail - gain;

      // Cap. It works (more or less)
      if ((to_battery[i] > cap) | (avail <= 0))  { // | (avail <= 0.1))
        break;
      }

      cost = comp * price;
      NumericVector icost = subset_range(cost, i, (horizon-1));

      NumericVector icost_real(horizon, icost[0]);
      icost_real = icost_real / deprec;

      if (is_false(any(icost > icost_real))) {
        break;
      }
    }
    balance[i] = comp[i] - comp_init[i];
    storage[i] = storage[i-1]*(1-depreciation) + balance[i];
  }

  // balance = comp - comp_init;
  // storage = cumsum(balance);


  NumericMatrix mtx = cbind(price, comp, comp_init, cost, cost_init, balance, storage);

  colnames(mtx) = CharacterVector::create("price", "comp", "comp_init",
           "cost", "cost_init", "balance", "storage");

  if (is<DatetimeVector>(xts_index(matrix))) {
    DatetimeVector time_index = xts_index(matrix);
    mtx.attr("index") = time_index;
    mtx.attr("class") = CharacterVector::create("xts", "zoo");
  }
  return mtx;
}


