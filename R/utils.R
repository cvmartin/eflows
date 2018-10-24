#' @import Rcpp
#' @import RcppArmadillo
#' @import R6
#' @useDynLib eflows
#' @importFrom stats as.formula
#' 


# data processing ---------------------------------------------------------
listify <- function(input) {
  if (is.list(input)) {return(input)}
  list(input)
}

`%||%` <- function (x, y){
  if (is.null(x)) y else x
}

# time --------------------------------------------------------------------
is_POSIXt <- function(x) inherits(x, "POSIXt")
is_date <- function(x) inherits(x, "Date")

freq_seconds <- function(vec) {
  if (is_date(vec) == TRUE) vec <- as.POSIXct(vec)
  if (is_POSIXt(vec) == FALSE) {
    warning("Can't coerce input to 'POSIXt' class")
    return(NULL)
  }
  mean(diff(as.integer(vec)))
}

# power - energy ----------------------------------------------------------
# kw_to_kwh <- function(df) {
#   stopifnot(inherits(df[[1]], "POSIXt"))
#   f <- freq_seconds(df[[1]])
#   cbind(
#     df[1],
#     apply(df[2:length(df)], 2, function(x){x * f})
#   )
# }
# 
# kwh_to_kw <- function(df) {
#   stopifnot(inherits(df[[1]], "POSIXt"))
#   f <- freq_seconds(df[[1]])
#   cbind(
#     df[1],
#     apply(df[2:length(df)], 2, function(x){x / f})
#   )
# }