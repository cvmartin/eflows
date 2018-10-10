#' Allocate energy flows to batteries.
#' 
#' Given a number of factors, the function returns the
#  demand and updated state of charge of the batteries.
#'
#' @param flow Energy flow to be exchanged in the following time step. 
#' Positive for charge, negative for discharge.
#' @param soc Current state of charge.
#' @param vol Objective state of charge to be achieved.
#' @param share Double, indicating the priority to share the energy flow.
#' @param level Integer, expressing the priority to be resolved, starting with the highest.
#' @param active boolean, expressing whether the battery should be considered
#' @param eff Efiiciency of the energy flow.
#' @param cap Maximum energy that can be exchanged witht the battery
#'
#' @return An object containing the state of charge of each battery
#' @export
#'
#' @examples
#' 1+1
allocate <- function(flow, soc, vol, share = 1, level = 1, active = 1, eff = 1, cap = 0) {

  .Call('_eflows_allocateCpp', PACKAGE = 'eflows', flow, soc, vol, share, level, active, eff, cap)
}
