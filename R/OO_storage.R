#' R6 class for the e_storage
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
e_storage <- R6Class("e_storage",
                    public = list(
                      input = NULL,
                      output = NULL,
                      initialize = function(input = list()) {
                        tolist <- listify(input)
                        lapply(tolist, function(x){assert_that(inherits(x, "storage"))})
                        names(tolist) <- lapply(tolist, function(x){x$name})
                        self$input <- tolist
                      }
                    )
)


#' Class to define a storage
#' 
#' @docType class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
#' @section Methods:
#' \describe{
#'   \item{\code{example_method(parameter_1 = 3)}}{This method uses \code{parameter_1} to ...}
#' }
storage <- R6Class("storage",
                    public = list(
                      name = NULL,
                      vol = NULL, 
                      soc = NULL, 
                      cap = list(to = NULL, 
                                 from = NULL),
                      eff = list(to = NULL,
                                 from = NULL),
                      self_discharge = NULL,
                      initialize = function(vol, 
                                            soc = 0, 
                                            name = NULL, 
                                            cap = list(to = NULL, from = NULL), 
                                            eff = list(to = 1, from = 1), 
                                            self_discharge = 0) {
                        
                        self$name <- name
                        self$vol <- vol
                        self$soc <- soc
                       
                        self$cap <- cap
                        
                        self$eff <- eff
                        
                        self$self_discharge <- self_discharge
                      }
                    )
)
