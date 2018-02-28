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
//' @param vol Objective state of charge to be achieved
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
                 NumericVector share = NumericVector::create(1),
                 NumericVector level = NumericVector::create(1),
                 LogicalVector active = LogicalVector::create(1),
                 NumericVector eff = NumericVector::create(1),
                 NumericVector cap = NumericVector::create(0)){
  double overflow = flow;

  int n = soc.length();
  // Rcout << "hi there 4";

  NumericVector avail =  vol - soc;

  IntegerVector active2 = as<IntegerVector>(active);
  if (active2.length() == 1) active2 = rep(active2[0], n);

  if (vol.length() == 1) vol = rep(vol[0], n);
  if (share.length() == 1) share = rep(share[0], n);
  if (level.length() == 1) level = rep(level[0], n);
  if (eff.length() == 1) eff = rep(eff[0], n);
  if (cap.length() == 1) cap = rep(cap[0], n);

  IntegerVector demand(n, 0);

  CharacterVector mrows = CharacterVector::create(
    "soc","vol","avail","share","level", "active", "eff", "cap", "demand");

  // Error: if the size of the vectors is not the right one.
  List elements = List::create(vol, share, level, active2, eff, cap);
  CharacterVector tocheck = CharacterVector::create(
    "vol","share","level", "active", "eff", "cap");
  for (int i=0; i < elements.size(); ++i) {
    NumericVector check = elements[i];
    if (check.size() != n) {
      std::string chekname = Rcpp::as<std::string>(tocheck[i]);
      stop("'%s': expected length either 1 or %s, instead of %s.",
           chekname, n, check.size());
    }
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
  rownames(m) = mrows;

  // cap: limit the vol if it exceeds the cap by the efficiency
  if (flow > 0){
    m(1, _) = ifelse((m(7, _) * m(6, _) < m(2, _))&(m(7, _) != 0),
      (m(0, _) + m(7, _)* m(6, _)),
      m(1, _));
    m(2, _) = m(1, _) - m(0, _);
  } else {
    m(1, _) = ifelse((m(7, _) / m(6, _) > m(2, _))&(m(7, _) != 0),
      (m(0, _) + m(7, _)/ m(6, _)),
      m(1, _));
    m(2, _) = m(1, _) - m(0, _);
  }

  // Error: if there are active and with available of opposite sign
  NumericVector checksign(m.ncol());
  for (int i=0; i < m.ncol(); ++i){
    if ((m(2, i) > 0) & (m(5, i) == 1)) {
      checksign[i] = 1;
    } else if ((m(2, i) < 0) & (m(5, i) == 1)){
      checksign[i] = -1;
    }
  }
  if (is_true(any(checksign == 1)) & is_true(any(checksign == -1))){
    stop("charge and discharge of batteries cannot be executed in the same call. Use 'relocate()' instead");
  }

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


