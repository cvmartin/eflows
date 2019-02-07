#' Define an e_frame object
#' 
#' @docType class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
#' @field serveraddress Stores address of your lightning server
#' @field sessionid Stores id of your current session on the server.
#' @field url Stores url of the last visualization created by this object.
#' @field autoopen Checks if the server is automatically opening the visualizations.
#' @field notebook Checks if the server is in the jupyter notebook mode.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/lightning-viz/lightining-r/}
#'   \item{\code{new(serveraddress)}}{This method is used to create object of this class with \code{serveraddress} as address of the server object is connecting to.}
#'   \item{\code{sethost(serveraddress)}}{This method changes server that you are contacting with to \code{serveraddress}.}
#'   \item{\code{createsession(sessionname = "")}}{This method creates new session on the server with optionally given name in \code{sessionname}.}
#'   \item{\code{usesession(sessionid)}}{This method changes currently used session on the server to the one with id given in \code{sessionid} parameter.}
#' }
e_frame <- R6Class("e_frame",
                    public = list(
                      
                      setup = list(time = list(series = NULL, 
                                               step = NULL), 
                                   units = list(energy = NULL, 
                                                price = NULL)),
                      production = NULL, 
                      demand = NULL, 
                      storage = NULL, 
                      infrastructure = NULL,
                      utility = list(
                        input = list(
                          fit = list(curve = NULL), 
                          price = NULL), 
                        output = list(
                          fit = list(curve = NULL)
                          )
                        ),
                      
                      initialize = function(timeseries, unit = "kWh") {
                        self$setup$units$energy <- unit
                        l <- length(timeseries)
                        if (is.null(freq_seconds(timeseries))) {
                          warning(sprintf("'time$series' set as a vector (1:%s)", l))
                          self$setup$time$series <- seq(1:l)
                          return(invisible(self))
                        }
                        self$setup$time$series <- timeseries
                        self$setup$time$step <- freq_seconds(timeseries)
                        return(invisible(self))
                      },
# set ---------------------------------------------------------------------
                      set_demand = function(obj){
                        assert_that(inherits(obj, "e_demand"))
                        self$demand <- obj
                        return(invisible(self))
                      },
                      set_production = function(obj){
                        assert_that(inherits(obj, "e_production"))
                        self$production <- obj
                        return(invisible(self))
                      },
                      set_price = function(vector, unit){
                        self$utility$input$price <- vector
                        self$setup$units$price <- unit
                        return(invisible(self))
                      },
                      set_cap = function(vector){
                        self$infrastructure$input$grid$capacity <- vector
                        return(invisible(self))
                      },
                      set_storage = function(obj){
                        assert_that(inherits(obj, "e_storage"))
                        self$storage <- obj
                        return(invisible(self))
                      },
# do: foreshift  ----------------------------------------------------------------
                      do_foreshift = function(add_input_vct = NULL, 
                                              fit = ~ 1*.demand){
                        
                        list_data <- lapply(self$demand$input$flex, function(x){x[["data"]]})
                        list_steps <- lapply(self$demand$input$flex, function(x){x[["steps"]]})
                        list_name <- lapply(self$demand$input$flex, function(x){x[["name"]]})
                        list_cap <- lapply(self$demand$input$flex, function(x){x[["cap"]]})
                        
                        init_input_vct <- list(.demand_fixed = self$demand$input$fixed %||% NULL, 
                                               .production_fixed = self$production$sum_fixed %||% NULL, 
                                               .price = self$utility$input$price %||% NULL, 
                                               .cap = self$infrastructure$input$grid$capacity %||% NULL)
                        
                        clean_input_vct <-  Filter(Negate((is.null)), init_input_vct)
                        total_input_vct <- c(clean_input_vct, add_input_vct)
                        
                        # if (all(all.vars(fit)) %in% names(total_input_vct)) # Raise error
                        
                        fshifted <- foreshift(
                          input_mtx = list_data,
                          cap = list_cap,
                          input_vct = total_input_vct,
                          flex_step = list_steps,
                          fit = fit
                        )
                        
                        build_output_flex <- function(data, steps, name, cap){
                          list(data = data,
                               steps = steps, 
                               name = name, 
                               cap = cap)
                        }
                        
                        self$demand$output$flex <- 
                          mapply(build_output_flex, 
                                 data = fshifted$demand_flex, 
                                 steps = lapply(fshifted$demand_flex, function(x){1:ncol(x)}), 
                                 name = list_name, 
                                 cap = list_cap, 
                                 SIMPLIFY = FALSE
                          )
                        
                        self$demand$output$fixed <- fshifted$demand_fixed
                        self$demand$output$unallocated <- fshifted$unallocated
                        
                        self$utility$input$fit$curve <- fshifted$fit_curve_initial
                        self$utility$output$fit$curve <- fshifted$fit_curve_final
                        
                        return(invisible(self))
                      },
# do:backshift ------------------------------------------------------------
                      do_backshift = function(add_input_vct = NULL,
                                              horizon = 8, 
                                              fit = ~ 1*.demand){
                        
                        init_input_vct <- list(.demand_fixed = self$demand$input$fixed %||% NULL, 
                                               .production_fixed = self$production$sum_fixed %||% NULL, 
                                               .price = self$utility$input$price %||% NULL, 
                                               .cap = self$infrastructure$input$grid$capacity %||% NULL)
                        
                        clean_input_vct <-  Filter(Negate((is.null)), init_input_vct)
                        total_input_vct <- c(clean_input_vct, add_input_vct)
                        
                        bshifted <- backshift(
                          input_consumption = self$demand$input$fixed,
                          horizon = horizon, 
                          storage = self$storage$input,
                          input_vct = total_input_vct,
                          fit = fit
                        )
                        
                        self$demand$output$bsh_pot <- bshifted$mtx_prebsh
                        self$demand$output$backshifted <- bshifted$mtx_postbsh
                        self$demand$output$fixed <- bshifted$final_consumption
                        
                        return(invisible(self))
                        
                      }
                    )
)

