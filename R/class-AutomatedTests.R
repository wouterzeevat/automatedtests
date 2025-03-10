#' AutomatedTest class
#'
#' @description The AutomatedTest class is the class that represent a result of a statistical test. It contains all the attributes of the results such as the p-value, degrees of
#' freedom, and more.
#'
#' @examples
#' AutomatedTest$new(
#'   test = "Unpaired T-test",
#' )
#'
#' @importFrom R6 R6Class
#'
AutomatedTest <- R6::R6Class(
  "AutomatedTest",
  private = list(
    # Private variables
    .test = character(0),

    # Private methods
    .setTest = function(test) {
      stopifnot(is.character(test))
      private$.test <- test
    }
  ),

  public = list(

    initialize = function(test) {
      private$.setTest(test)

    },

    getTest = function() {
      return(private$.test)
    }
  )
)
