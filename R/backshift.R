#' Shift demand to the past
#'
#' @param input_vct raw consumption
#' @param horizon how much do you look back
#' @param storage list of R6 class storage
#' @param fit formula to calulate the fit of the flexible demand
#'
#' @return object 
#' \describe{
#'   \item{demand fixed}{First item}
#'   \item{demand flex}{Second item}
#'   \item{fit curve initial}{Second item}
#'   \item{fit curve final}{Second item}   
#' }
#' @export
#' @examples
#' 1+1
backshift <- function(input_vct,
                      horizon,
                      storage = list(),
                      fit = ~ 1*.demand){
  
  testing_storage <- storage[[1]]
  
  sol <- backshiftCpp(input_vct, 
                      testing_storage$self_discharge, 
                      testing_storage$eff, 
                      horizon)
  sol
}
