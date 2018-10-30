#' @import Rcpp
#' @import RcppArmadillo
#' @import R6
#' @import assertthat
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


# safe environment --------------------------------------------------------

safe_f <- c(
  getGroupMembers("Math"),
  getGroupMembers("Arith"),
  getGroupMembers("Compare"),
  getGroupMembers("Logic"),
  "<-", "{", "(", "ifelse"
)

safe_env <- new.env(parent = emptyenv())

for (f in safe_f) {
  safe_env[[f]] <- get(f, "package:base")
}
