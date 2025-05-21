#' AutomatedTest class
#'
#' @description The AutomatedTest class represents a result of a statistical test. It contains attributes such as the p-value, degrees of freedom, and more.
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
    .paired = NULL,

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
    #' @param compare_to Numeric value to compare to for comparison in one-sample tests. Default is NULL.
    #' @param paired Logical; if TRUE, the test will be performed as paired if applicable. Default is FALSE.
    initialize = function(data, identifiers, compare_to = NULL, paired = FALSE) {
      private$.data <- data
      private$.identifiers <- identifiers
      private$.compare_to <- compare_to
      private$.paired <- paired

      private$.setTest(pick_test(test_object = self))
      private$.setResult(get_test_from_string(test_object = self))
    },

    #' @description Get the data used in the test
    #' @return A dataframe with all features
    getData = function() {
      return(private$.data)
    },

    #' @description Shows if the data is paired, if there are multiple rows with the same identifier, the data has more
    #' samples (TIDY DATA). Making the data paired.
    #'
    #' @return Whether the data is paired (TRUE/FALSE).
    isPaired = function() {
      if (private$.paired) {
        return(TRUE)
      }

      # If any id exists twice the identifiers have to be used
      if (length(private$.identifiers) > 1) {
        return(any(duplicated(private$.identifiers)))
      }
      return(FALSE)
    },

    #' @description A list of the identifiers used for the data
    #' @return Returns the identifiers
    getIdentifiers = function() {
      return(private$.identifiers)
    },

    #' @description Get the comparison value for one-sample tests
    #' @return A numeric value for comparison
    getCompareTo = function() {
      return(private$.compare_to)
    },

    #' @description Updates the compare_to variable. Is public because the
    #' compare value can get changed depending on the type of test.
    #' @param compare_to Numeric value to compare to.
    #' @return Updated object with comparison value set.
    setCompareTo = function(compare_to) {
      private$.compare_to <- compare_to
    },

    #' @description Get the data types of the features in the object
    #' @return A list of data types (e.g., Quantitative or Qualitative)
    getDatatypes = function() {
      result <- c()
      for (feature in names(self$getData())) {
        if (is.numeric(self$getData()[[feature]]) && !all((unique(self$getData()[[feature]]) %in% c(0,1)))) {
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

    #' @description Get the result of selected statistical test
    #' @return The result of the statistical test
    getResult = function() {
      return(private$.result)
    },

    #' @description Whether the test results are significant or not.
    #' @return TRUE / FALSE depending on the significance of the test.
    isSignificant = function() {
      return(self$getResult()$p.value < 0.05)
    },

    #' @description Print a summary of the test object
    print = function() {
      cat("Automated Test:\n")
      cat("Data: ", paste0(colnames(self$getData()), collapse = ", "), "\n")

      # If one sample test
      size <- ncol(self$getData())
      if (size == 1) {
        cat("Compared to: ", self$getCompareTo(), "\n")
      }

      cat("Test: ", self$getTest(), "\n")
      cat("Results:\n  p.value: ", self$getResult()$p.value, "\n")
      cat("  Significant: ", self$isSignificant(), "\n")
    }
  )
)
