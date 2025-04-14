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
    .subsets = c(),
    .paired = logical(0),
    .compare_to = numeric(0),
    .parametric_list = list(),
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
    #' @param subsets A vector with subsets in the data.
    #' @param compare_to A numeric value for comparison in one-sample tests.
    initialize = function(data, subsets, paired, compare_to) {
      private$.data <- data
      private$.subsets <- subsets
      private$.paired <- paired
      private$.compare_to <- compare_to

      # Apply parametric tests
      parametric_results <- list()
      if (is.null(subsets)) {
        for (feature in colnames(data)) {
          parametric_results[[feature]] <- check_parametric(data[[feature]])
        }
      } else {
        for (subset in unique(subsets)) {
          for (feature in colnames(data)) {
            parametric_results[[paste0(feature, "-", subset)]] <- check_parametric(data[subsets == subset, feature])
          }
        }
      }
      private$.setParametricList(parametric_results)
      private$.setTest(pick_test(test_object = self))
      #cat(self$getTest())
      private$.setResult(get_test_from_string(self))
    },

    #' @description Get the data used in the test
    #' @return A dataframe with the data except the subsets
    getData = function() {
      return(private$.data)
    },

    #' @description Get the subsets used in the test
    #' @return A vector containing subset information
    getSubsets = function() {
      return(private$.subsets)
    },

    #' @description Get the paired variable
    #' @return Whether the paired variable is True or False
    isPaired = function() {
      return(private$.paired)
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
      if (!is.null(self$getSubsets())) {
        result <- append(result, "Qualitative")
      }
      return(result)
    },

    #' @description Get the parametric test results of the features and subsets
    #' @return A list of parametric test results
    getParametricList = function() {
      return(private$.parametric_list)
    },

    #' @description Check if the data meets parametric assumptions
    #' @return TRUE if parametric assumptions are met, otherwise FALSE
    isParametric = function() {
      for (test in self$getParametricList()) {
        if (typeof(test) == "logical" && !test) return(FALSE)
      }
      result <- sapply(self$getParametricList(), function(x) x$result == TRUE)
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

      # If subsets exist
      if (!is.null(self$getSubsets())) {
        cat("Subsets: ", unique(self$getSubsets()), "\n")
      }

      # If one sample test
      size <- ncol(self$getData())
      if(!is.null(self$getSubsets())) {
        size <- size + 1
      }

      if (size == 1) {
        cat("Compared to: ", self$getCompareTo(), "\n")
      }

      cat("Test: ", self$getTest(), "\n")
      cat("Results:\n  p.value: ", self$getResult()$p.value, "\n")
      cat("  Significant: ", self$getResult()$p.value < 0.05)
    }
  )
)
