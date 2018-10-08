#' Shift demand to the future
#'
#' @param input_mtx list of flexible matrices
#' @param flex_step list of flexibility steps
#' @param input_vct list of input vectorized
#' @param fit formula to calulate the fit of the flexible demand
#'
#' @return
#' @export
#'
#' @examples
foreshift <- function(input_mtx,
                       flex_step,
                       input_vct,
                       fit = ~ 1*.demand
){

  # You can also pass the formula as a character
  if (is.character(fit)) fit <- as.formula(fit)

  # Make all the input a list of matrices
  # that can be made a cube ("stacked")
  format_mtx_steps <- function(matrices, steps) {
    listify <- function(input) {
      if (is.list(input)) {return(input)}
      list(input)
    }

    matrices <- listify(matrices)
    steps <- listify(steps)

    mapply(formatFlexSteps,
           matrices,
           steps,
           MoreArgs = list(max_step = max(sapply(steps, max))),
           SIMPLIFY = FALSE
    )
  }
  mtx_list <- format_mtx_steps(input_mtx, flex_step)


  # .demand is a keywork in the fit formula, as it is
  # .demand_fixed passed into input_vct

  if (".demand_fixed" %in% names(input_vct)) {
    input_vct[[".demand"]] <- input_vct[[".demand_fixed"]]
  } else {
    input_vct[[".demand"]] <- rep(0, nrow(mtx_list[[1]]))
  }

  # .cap_flex and .cap_demand. Both are important.
  if (!(".cap_flex" %in% names(input_vct))) {
    input_vct[[".cap_flex"]] <- rep(0, nrow(mtx_list[[1]]))
  }

  if (!(".cap_demand" %in% names(input_vct))) {
    input_vct[[".cap_demand"]] <- rep(0, nrow(mtx_list[[1]]))
  }


  # the list input_vct is instead an environment
  env_fit <- list2env(input_vct)
  ## input verification
  # stopifnot(
  #   # not all the variables for the fit formula are passed
  #   # to the input_vct list
  #   all(all.vars(fit) %in% names(env_fit)),
  #   # not all the variables in the fit environment have
  #   # the same length as the matrix passed
  #   all(sapply(names(env_fit),
  #              function(x){
  #                length(env_fit[[x]]) == nrow(input_mtx)
  #                })
  #       ),
  #   # the length of the flex_step does not match the column
  #   # of the flex demand
  #   ncol(input_mtx) == length(flex_step)
  #   )
  ##

  expr_fit <- fit[[2]]

  aux_demand <- ~ 1*.demand

  aux_env = new.env()

  expr_aux_demand <- aux_demand[[2]]

  sol <- foreShiftCpp(mtx_list,
                      env_fit,
                      expr_fit,
                      aux_env,
                      expr_aux_demand)

  sol

  # list(demand_fixed = sol$demand_fixed,
  #      demand_flex = sol$demand_flex,
  #      fit_curve_initial = sol$fit_curve_initial,
  #      fit_curve_final = sol$fit_curve_final)

}
