#include <Rcpp.h>
#include"../inst/include/eflowsheader.hpp"
using namespace Rcpp;

// [[Rcpp::plugins("cpp11")]]

// [[Rcpp::export]]
DataFrame model_c(DataFrame df,
                  DataFrame df_battery,
                  double initial_soc,
                  double to_battery_eff,
                  double from_battery_eff,
                  double to_ev_eff,
                  double from_ev_eff,
                  double max_battery_rate,
                  double ev_priority_charge = 0.5,
                  double ev_priority_discharge = 0.5,
                  double grid_capacity = 10,
                  bool use_grid_cap = true,
                  bool use_v2g = true,
                  double charge_thold = 0.7,
                  double v2g_thold = 0.9,
                  double pref_charge_stationary = 0.5,
                  double pref_discharge_stationary = 0.5,
                  double pref_charge_ev = 0.5,
                  double pref_discharge_ev = 0.5){


  // Arrange the preferences

  NumericVector pref_charge = NumericVector::create(pref_charge_stationary, pref_charge_ev);
  NumericVector pref_discharge = NumericVector::create(pref_discharge_stationary, pref_discharge_ev);

  NumericVector actual_pref_discharge = NumericVector::create(0.5, 0.5);


  NumericVector balance = df["balance"];
  NumericVector direct_solar = df["direct_solar"];

  // layout for empty (zeroes) vector
  int n = df.nrow();
  NumericVector from_battery(n);
  NumericVector to_battery(n);
  NumericVector from_ev(n);
  NumericVector to_ev(n);
  NumericVector to_grid(n);
  NumericVector from_grid(n);
  NumericVector grid_to_ev(n);

  NumericVector distr (3);
  NumericVector soc_global (2);
  NumericVector cap_global (2);



  double actual_soc = 0;


  // import conditionally: infrastrcture
  NumericVector potential_grid_to_ev(n);
  if (df.containsElementNamed("potential_grid_to_ev")){
    potential_grid_to_ev = df["potential_grid_to_ev"];
  }


  // import conditionally: batteries
  NumericVector stationary_soc(n);
  NumericVector stationary_capacity(n);
  if (df_battery.containsElementNamed("stationary_capacity")){
    stationary_capacity = df_battery["stationary_capacity"];
    stationary_soc[0] = stationary_capacity[0] * initial_soc;
  }


  NumericVector ev_capacity(n);
  NumericVector ev_in(n);
  if (df_battery.containsElementNamed("ev_capacity")){
    ev_capacity = df_battery["ev_capacity"];
    ev_in = df["ev_in"];
  }
  NumericVector ev_soc(n);

  // LOOP
  for (int i=1; i < n; ++i){

    // calculate the actual soc of the ev battery when vehicles dis/connect
    actual_soc = ev_soc[i-1];
    if (ev_capacity[i] < ev_capacity[i-1]) {
      actual_soc = ((ev_soc[i-1] * ev_capacity[i])/ev_capacity[i-1]);
    }
    if (ev_capacity[i] > ev_capacity[i-1]) {
      actual_soc = ((ev_soc[i-1] + ev_in[i]));
    }
    // To correct: ensure that when vehicle swapping, the theoretical soc
    // cannot go over the actual capacity
    if (actual_soc > ev_capacity[i]){
      actual_soc = ev_capacity[i];
    }

    soc_global = NumericVector::create(stationary_soc[i-1], actual_soc);
    cap_global = NumericVector::create(stationary_capacity[i], ev_capacity[i]);

    // > redis(flow = 90, pref_v = 1, soc_v = 34, cap_v = 50)
    //   [1] 16 74
    // > allocate(flow = 90, soc = 34, vol = 50, share = 1)


    if (balance[i] > 0) {
      distr = allocate(balance[i], soc_global, cap_global, pref_charge,
                       1,1,1,0);
      to_battery[i] = distr[0];
      to_ev[i] = distr[1];
      to_grid[i] = distr[2];
    }

    if (balance[i] < 0) {
      // when it is impossible for the vehicle to discharge
      // if ((use_v2g == false) | (ev_soc[i] / ev_capacity[i] < v2g_thold)){
      if (use_v2g == false){
        actual_pref_discharge = NumericVector::create(1,0);
      } else {
        actual_pref_discharge = pref_discharge;
      }

      distr = allocate(fabs(balance[i]), (cap_global - soc_global), cap_global, actual_pref_discharge,
                       1,1,1,0);
      from_battery[i] = distr[0];
      from_ev[i] = distr[1];
      from_grid[i] = distr[2];

      if ((use_v2g == false) | (ev_soc[i] / ev_capacity[i] < v2g_thold)){
        // if (use_v2g == false){
        // To prevent withdraw from the EV battery
        from_ev[i] = 0;
        from_grid[i] = distr[1] + distr[2];
      }
    }

    // New state of charge of the stationary battery
    // When it is almost full, finish it.
    if (stationary_soc[i-1] + to_battery[i] - from_battery[i] == stationary_capacity[i]){
      stationary_soc[i] = stationary_capacity[i]
      - (from_battery[i]* (1 - from_battery_eff));
    } else {
      stationary_soc[i] = stationary_soc[i-1] + to_battery[i] - from_battery[i]
      - (to_battery[i] * (1 - to_battery_eff))
      - (from_battery[i]* (1 - from_battery_eff));
    }
    if (ev_soc[i-1] + to_ev[i]  - from_ev[i]== ev_capacity[i]){
      ev_soc[i] = ev_capacity[i]
      - (from_ev[i]* (1 - from_ev_eff));
    } else {
      ev_soc[i] = actual_soc + to_ev[i] - from_ev[i]
      - (to_ev[i] * (1 - to_ev_eff))
      - (from_ev[i]* (1 - from_ev_eff));
    }

    // calculate grid_to_ev
    if (ev_soc[i]/ev_capacity[i] < charge_thold) {
      grid_to_ev[i] = ev_capacity[i] - ev_soc[i];
      if (grid_to_ev[i] > potential_grid_to_ev[i]) grid_to_ev[i] = potential_grid_to_ev[i];
      if ((grid_to_ev[i] + from_grid[i] > grid_capacity) & (use_grid_cap == true)) grid_to_ev[i] = grid_capacity - from_grid[i];
      ev_soc[i] = ev_soc[i] + grid_to_ev[i];
    }

    // Ensure the batteries cannot fall below zero
    if (stationary_soc[i] < 0) stationary_soc[i] = 0;
    if (ev_soc[i] < 0) ev_soc[i] = 0;

  }

  DataFrame result_df = DataFrame::create(_["balance"]= balance,
                                          _["direct_solar"]= direct_solar,
                                          _["from_battery"]= from_battery,
                                          _["to_battery"]= to_battery,
                                          _["from_ev"]= from_ev,
                                          _["to_ev"]= to_ev,
                                          _["grid_to_ev"]= grid_to_ev,
                                          _["from_grid"]= from_grid,
                                          _["to_grid"]= to_grid);

  if (df_battery.containsElementNamed("stationary_capacity")){
    result_df.push_back(stationary_soc, "stationary_soc");
  }

  if (df_battery.containsElementNamed("ev_capacity")){
    result_df.push_back(ev_soc, "ev_soc");
  }

  return(result_df);
}
