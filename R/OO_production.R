#' R6 class for the e_production
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
e_production <- R6Class("e_production",
                    public = list(
                      input = list(fixed = NULL,
                                   flex = NULL),
                      output = NULL,
                      initialize = function(fixed = NULL,
                                            flex = NULL) {
                        self$input$fixed <- listify(fixed)
                        self$input$flex <- listify(flex)
                      }
                    ), 
                    active = list(
                      sum_fixed = function(){
                        Reduce(`+`, self$input$fixed)
                      }
                    )
)
