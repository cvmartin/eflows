#' An example R6 class
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
e_demand <- R6Class("e_demand",
                    public = list(
                      input = list(fixed = NULL,
                                   flex = NULL),
                      output = NULL,
                      # fixed = NULL,
                      # flex = NULL,
                      initialize = function(fixed = c(),
                                            flex = NULL) {
                        # if(inherits(flex, "flex_mtx") == FALSE) stop("flexible demand can only be 'flex_mtx' objects")
                        listify <- function(input) {
                          if (is.list(input)) {return(input)}
                          list(input)
                        }
                        
                        self$input$fixed <- fixed
                        self$input$flex <- listify(flex)
                      },
                      do_foreshift = function(fit = ~ 1*.demand){
                        
                        list_data <- lapply(self$input$flex, function(x){x[["data"]]})
                        list_steps <- lapply(self$input$flex, function(x){x[["steps"]]})
                        list_name <- lapply(self$input$flex, function(x){x[["name"]]})
                        
                        fshifted <-
                          foreshift(input_mtx = list_data,
                                    input_vct = list(.demand_fixed = self$input$fixed),
                                    flex_step = list_steps,
                                    fit
                          )
                        names(fshifted$demand_flex) <- list_name
                        self$output <- fshifted
                        return(invisible(self))
                      }
                    )
)


#' Another example of an R6 class
#' 
#' @docType class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
#' @section Methods:
#' \describe{
#'   \item{\code{example_method(parameter_1 = 3)}}{This method uses \code{parameter_1} to ...}
#' }
flex_mtx <- R6Class("flex_mtx",
                    public = list(
                      name = NULL,
                      data = NULL,
                      steps = NULL,
                      initialize = function(data = matrix(),
                                            steps = c(),
                                            name = NULL) {
                        
                        # if(length(steps) != ncol(data)) stop("steps and columns in data do not match")
                        
                        self$name <- name
                        self$data <- data
                        self$steps <- steps
                      }
                    )
)

