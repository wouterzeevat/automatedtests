#' AutomatedTest class
#'
#' @description The AutomatedTest class represents a result of a statistical test. It contains attributes such as the p-value, degrees of freedom, and more.
#'
#'
#' @importFrom R6 R6Class
#' @export
AutomatedTest <- R6::R6Class(
  "AutomatedTest",
  private = list(
    # Private variables
    .data = data.frame(),
    .identifiers = list(),
    .compare_to = numeric(0),
    .test = character(0),
    .result = NULL,

    # Private methods
    .setTest = function(test) {
      stopifnot(is.character(test))
      private$.test <- test
    },

    .setResult = function(result) {
      private$.result <- result
    },

    .setParametricList = function(parametric) {
      private$.parametric_list <- parametric
    }
  ),

  public = list(

    #' @description Initialize an instance of the AutomatedTest class
    #' @param data A dataframe containing the data for the test.
    #' @param identifiers A vector with the identifiers.
    #' @param compare_to value to compare to for comparison in one-sample tests.
    initialize = function(data, identifiers, compare_to = NULL) {
      private$.data <- data
      private$.identifiers <- identifiers
      private$.compare_to <- compare_to

      private$.setTest(pick_test(test_object = self))
      private$.setResult(get_test_from_string(self))
    },

    #' @description Get the data used in the test
    #' @return A dataframe with all features
    getData = function() {
      return(private$.data)
    },

    #' @description Get the identifier variable
    #' @return Whether the data has an identifier, if not
    #' The test is always UNPAIRED
    hasIdentifiers = function() {
      return(length(private$.identifiers) > 1)
    },

    #' @description A list of the identifiers used for the data
    #' @return Returns the identifiers
    #' The test is always UNPAIRED
    getIdentifiers = function() {
      return(private$.identifiers)
    },

    #' @description Get the comparison value for one-sample tests
    #' @return A numeric value for comparison
    getCompareTo = function() {
      return(private$.compare_to)
    },

    #' @description updates the compare_to variable. Is public because the
    #' compare value, can get changed depending on the type of test.
    setCompareTo = function(compare_to) {
      private$.compare_to <- compare_to
    },


    #' @description Get the data types of the features in the object
    #' @return A list of datatypes (e.g., Quantitative or Qualitative)
    getDatatypes = function() {
      result <- c()
      for (feature in names(self$getData())) {
        if (is.numeric(self$getData()[[feature]])) {
          result <- append(result, "Quantitative")
        } else {
          result <- append(result, "Qualitative")
        }
      }
      return(result)
    },

    #' @description Get the parametric test results of the features
    #' @return A list of parametric test results
    getParametricList = function() {
      parametric_list <- list()
      for (name in colnames(self$getData())) {
        feature <- self$getData()[[name]]
        parametric_list[[length(parametric_list) + 1]] <- check_parametric(feature)
      }
      return(parametric_list)
    },

    #' @description Check if the data meets parametric assumptions
    #' @return TRUE if parametric assumptions are met, otherwise FALSE
    isParametric = function() {
      result <- sapply(self$getParametricList(), function(x) {
        if (is.null(x)) {
          return(NULL)  # Skip this iteration if x is NULL
        }
        return(x$result == TRUE)
      })
      return(all(result))
    },

    #' @description Get the statistical test that was chosen
    #' @return The name of the statistical test
    getTest = function() {
      return(private$.test)
    },

    #' @description Get the reslult of selected statistical test
    #' @return The result of the statistical test
    getResult = function() {
      return(private$.result)
    },


    #' @description Print a summary of the test object
    print = function() {
      cat("Automated Test:\n")
      cat("Data: ", colnames(self$getData()), "\n")

      # If one sample test
      size <- ncol(self$getData())
      if (size == 1) {
        cat("Compared to: ", self$getCompareTo(), "\n")
      }

      cat("Test: ", self$getTest(), "\n")
      cat("Results:\n  p.value: ", self$getResult()$p.value, "\n")
      cat("  Significant: ", self$getResult()$p.value < 0.05)
    }
  )
)
