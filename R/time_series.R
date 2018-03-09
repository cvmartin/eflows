#' Convert Data Frame to a Time Series object.
#'
#' This is a random description.
#'
#' These are random details. Will it work somehow?
#' I will know it soon.
#'
#' @param df Data frame where the first column is a POXIXct objet or convertible.
#' @param ts Time series

#' @return Time series indexed by the first column of df.
#' @export
df_to_ts <- function(df) {
  new_ts <- xts::xts(x = df[, 2:(length(colnames(df)))], order.by = df[[1]])
  if (is.null(colnames(new_ts))) colnames(new_ts) <- colnames(df[2])
  return(new_ts)
}


#' @rdname df_to_ts
#' @return Data frame where the fist column is a POSIXt object calle "datetime"
#' @export
ts_to_df <- function(ts){
  new_df <- data.frame(datetime=zoo::index(ts),
                       zoo::coredata(ts))
}
