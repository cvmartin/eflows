#' @import Rcpp
#' @import RcppArmadillo
#' @import R6
#' @useDynLib eflows#'

.onUnload <- function (libpath) { library.dynam.unload("eflows", libpath)}

freq_hour <- function(vec) {
  sec_interval <- mean(diff(as.integer(vec)))
  sec_interval / 3600
}

# data frame - time series ------------------------------------------------


# power - energy ----------------------------------------------------------
kw_to_kwh <- function(df) {
  stopifnot(inherits(df[[1]], "POSIXt"))
  f <- freq_hour(df[[1]])
  cbind(
    df[1],
    apply(df[2:length(df)], 2, function(x){x * f})
  )
}

kwh_to_kw <- function(df) {
  stopifnot(inherits(df[[1]], "POSIXt"))
  f <- freq_hour(df[[1]])
  cbind(
    df[1],
    apply(df[2:length(df)], 2, function(x){x / f})
  )
}


