#' Define an e_frame object
#' 
#' @docType class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
#' @field serveraddress Stores address of your lightning server.
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
e_frame <- R6Class("e_demand",
                    public = list(
                      time = list(series = NULL, 
                                  step = NULL), 
                      production = NULL, 
                      demand = NULL, 
                      storage = NULL, 
                      infrastructure = NULL,
                      
                      initialize = function(timeseries) {
                        l <- length(timeseries)
                        if (is.null(freq_seconds(timeseries))) {
                          warning(sprintf("'time$series' set as a vector (1:%s)", l))
                          self$time$series <- seq(1:l)
                          return(invisible(self))
                        }
                        self$time$series <- timeseries
                        self$time$step <- freq_seconds(timeseries)
                        return(invisible(self))
                      },
                      
                      set_demand = function(obj){
                        self$demand <- obj
                        return(invisible(self))
                      },
                      
                      do_foreshift = function(...){
                        self$demand$do_foreshift(...)
                        return(invisible(self))
                      }
                        
                        
                      
                    )
)

