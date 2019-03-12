#' Shift demand to the past
#'
#' @param input_vct raw consumption
#' @param horizon how much do you look back
#' @param params_df list of R6 class storage
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
backshift <- function(input_consumption,
                      horizon,
                      params_df = as.data.frame(list()),
                      input_vct,
                      fit = ~ 1*.demand){
  
 
  # You can also pass the formula as a character
  if (is.character(fit)) fit <- as.formula(fit)
  
  if (".demand_fixed" %in% names(input_vct)) {
    input_vct[[".demand"]] <- input_vct[[".demand_fixed"]]
  } else {
    input_vct[[".demand"]] <- rep(0, nrow(mtx_list[[1]]))
  }
  
  # if (".demand_fixed" %in% names(input_vct)) {
  #   input_vct[[".demand"]] <- input_vct[[".demand_fixed"]]
  # } else {
  #   input_vct[[".demand"]] <- rep(0, nrow(mtx_list[[1]]))
  # }  
  
  env_fit <- list2env(input_vct, parent = safe_env)
  
  call_fit <- fit[[2]]
  env_aux = new.env(parent = safe_env)
  call_aux <- (~ 1*.demand)[[2]]
  
  sol <- backshiftCpp(consumption = input_consumption, 
                      params_df = params_df,
                      horizon = horizon,
                      env_fit = env_fit,
                      call_fit = call_fit,
                      env_aux = env_aux,
                      call_aux = call_aux)
  sol
}
