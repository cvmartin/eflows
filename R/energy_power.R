#' Switch between power and energy based objects
#'
#' In energy analysis, data is sometimes provided
#' power-based, when it is needed energy based.
#' These functions allow to swith between the two modes.
#'
#'
#' @param df Data frame to be transformed

#' @return Data frame where the columns have been time-transformed
#' to signify energy or power transfromations.
#' @export
kw_to_kwh <- function(df) {
  stopifnot(inherits(df[[1]], "POSIXt"))
  f <- freq_hour(df[[1]])
  cbind(
    df[1],
    apply(df[2:length(df)], 2, function(x){x * f})
  )
}


#' @rdname kw_to_kwh
#' @export
kwh_to_kw <- function(df) {
  stopifnot(inherits(df[[1]], "POSIXt"))
  f <- freq_hour(df[[1]])
  cbind(
    df[1],
    apply(df[2:length(df)], 2, function(x){x / f})
  )
}



