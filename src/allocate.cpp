#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::plugins("cpp11")]]

//' Allocate energy flows to batteries.
//'
//' Given a number of factors, the function returns the
//' demand and updated state of charge of the batteries.
//' @param flow Energy flow to be exchanged in the following time step .
//' Positive for charge, negative for discharge.
//' @param soc Current state of charge.
//' @param vol Objective state of charge to be achieved.
//' @param share Double, indicating the priority to share the energy flow
//' @param level Integer, expressing the priority to be resolved, starting with
//' the highest
//' @param active boolean, expressing whether the battery should be considered
//' @param eff Efiiciency of the energy flow.
//' @param cap Maximum energy that can be exchanged witht the battery
//'
//' @return An object containing the state of charge of each battery, its final
//' energy flow, and a double containing the energy that was not shared
//' @export
// [[Rcpp::export]]
List allocate (double flow,
                 NumericVector soc,
                 NumericVector vol,
                 NumericVector share = NumericVector::create(),
                 NumericVector level = NumericVector::create(),
                 LogicalVector active = LogicalVector::create(),
                 NumericVector eff = NumericVector::create(),
                 NumericVector cap = NumericVector::create()){
  double overflow = flow;

  int n = soc.length();
  Rcout << "hi there 4";

  NumericVector avail =  vol - soc;

  if (vol.length() == 1) vol = rep(vol[0], n);

  if (share.length() == 0) share = rep(1, n);
  if (share.length() == 1) share = rep(share[0], n);

  if (level.length() == 0) level = rep(1, n);
  if (level.length() == 1) level = rep(level[0], n);

  if (eff.length() == 0) eff = rep(1, n);
  if (eff.length() == 1) eff = rep(eff[0], n);

  if (cap.length() == 0) cap = rep(0, n);
  if (cap.length() == 1) cap = rep(eff[0], n);

  IntegerVector active2(n, 1);
  if (active.length() > 0 ) active2 = as<IntegerVector>(active);
  if (active2.length() == 1) active2 = rep(active2[0], n);

  IntegerVector demand(n, 0);

  if (vol.length() != n || share.length() != n || level.length() != n
        || active2.length() != n || eff.length() != n || cap.length() != n){
    stop("vectorized parameters must have the same length, or length 1");
  }
  // Define the master matrix
  NumericMatrix m(9, n);
  m(0, _) = soc;
  m(1, _) = vol;
  m(2, _) = avail;
  m(3, _) = share;
  m(4, _) = level;
  m(5, _) = active2;
  m(6, _) = eff;
  m(7, _) = cap;
  m(8, _) = demand;
  rownames(m) = CharacterVector::create("soc","vol","avail","share","level",
           "active", "eff", "cap", "demand");

  NumericVector n_levels = rev(sort_unique(m(4, _)));

  for (int i=0; i < n_levels.length(); ++i){

    // Create matrix by level
    IntegerVector posit = IntegerVector::create();
    for (int j=0; j < m.ncol(); ++j){
      if (m(4,j) == n_levels[i] && m(5,j) == 1) posit.push_back(j);
    }
    NumericMatrix mx(m.nrow(), posit.length());
    for (int j=0; j < posit.length(); ++j){
      mx(_,j) = m(_,posit[j]);
    }

    if (is_true(any(mx(2, _) > 0)) && is_true(any(mx(2, _) < 0))){
      stop("you cannot mix consumption and production");
    }

    while(is_true(any(mx(2, _) != 0))){
      // Redo share
      for (int j=0; j < mx.ncol(); ++j){
        if (mx(2, j) == 0) mx(3, j) = 0;
      }
      if(is_true(all(mx(3, _) == 0))) {
        for (int j=0; j < mx.ncol(); ++j){
          if (mx(2, j) != 0) mx(3, j) = 1;
        }
      }
      mx(3, _) = mx(3, _)/sum(mx(3, _));

      // // introduce the cap
      // for (int j=0; j < mx.ncol(); ++j){
      //   if((mx(7, j) > 0) && (mx(7, j) < mx(2, j))) {
      //     mx(2, j) = mx(7, j);
      //   }
      // }

      print(mx);

      // Calculate efficiency
      NumericVector eff_avail(mx.ncol());
      if (flow > 0){
        eff_avail = mx(2, _) * (1/mx(6, _));
      } else {
        eff_avail = mx(2, _) / (1/mx(6, _));
      }


      NumericVector d_init = mx(3, _) * overflow;
      NumericVector d_real = ifelse(abs(eff_avail) < abs(d_init), eff_avail, d_init);

      if (flow > 0){
        mx(0, _) = mx(0, _) + d_real * mx(6, _);
      } else {
        mx(0, _) = mx(0, _) + d_real / mx(6, _);
      }

      mx(2, _) = mx(1, _) - mx(0, _);
      mx(8, _) = mx(8, _) + d_real;
      overflow = overflow - sum(d_real);

      if (overflow == 0) break;
    }
    for (int j=0; j < posit.length(); ++j){
      m( _, posit[j]) = mx( _, j);
    }
    if (overflow == 0) break;
  }

  List result = List::create(_["soc"] = m(0, _),
                             _["flow"] = m(8, _),
                             _["overflow"] = overflow);

  return result;
}


