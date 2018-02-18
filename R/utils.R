freq_hour <- function(vec) {
  sec_interval <- mean(diff(as.integer(vec)))
  sec_interval / 3600
}

#' @export
allocate <- function(flow, pref_v, soc_v, cap_v) {
  flow_zero <- flow
  pref_v <- pref_v/sum(pref_v)

  available <- cap_v - soc_v
  demand_initial <- ifelse(pref_v == 0, 0, pref_v * flow)
  demand_real <- ifelse(demand_initial > available, available, demand_initial)
  overflow <- ifelse(demand_initial > available, demand_initial - available, 0)
  demand_final <- demand_real

  while (any(overflow > 0)) {
    flow <- flow_zero - sum(demand_final)
    soc_v <- soc_v + demand_real
    available <- cap_v - soc_v
    if ( sum(available) == 0){
      break
    }
    # this line is potentially problematic
    pref_v <- ifelse(overflow > 0, 0, ifelse(pref_v == 0 && demand_real == 0, 1, pref_v))
    pref_v <- pref_v/sum(pref_v)
    demand_initial <- ifelse(pref_v == 0, 0, pref_v * flow)
    demand_real <- ifelse(demand_initial > available, available, demand_initial)
    overflow <- ifelse(demand_initial > available, demand_initial - available, 0)
    demand_final <- demand_final + demand_real
  }

  return(c(demand_final, sum(overflow)))
}
