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

      parametric_list <- data.frame(
        Feature = character(),
        result = logical(),
        p_value = numeric(),
        test = logical(),
        statistic = numeric(),
        stringsAsFactors = FALSE
      )

      for (name in colnames(self$getData())) {
        feature = self$getData()[[name]]
        result = check_parametric(feature)

        df = data.frame(
          Feature = name,
          result = result$result,
          p_value = result$p_value,
          test = result$test,
          statistic = result$statistic,
          stringsAsFactors = FALSE
        )

        parametric_list = rbind(parametric_list, df)
      }
      rownames(parametric_list) <- NULL
      return(parametric_list)
    },

    #' @description Check if the data meets parametric assumptions
    #' @return TRUE if parametric assumptions are met, otherwise FALSE
    isParametric = function() {
      return(all(self$getParametricList()$result))
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

    #' @description Get the strength(s) of selected statistical test.
    #' @return A named numeric value indicating the strength of the result.
    #' The type and meaning depend on the test used:
    #' \describe{
    #'   \item{coefficient}{Effect size and direction of predictors in regression}
    #'   \item{r}{Correlation strength and direction}
    #'   \item{mean difference}{Difference in group means}
    #'   \item{statistic}{Test statistic measuring group difference or association}
    #'   \item{F statistic}{Ratio of variances across groups}
    #'   \item{proportion}{Estimated success rate in the sample}
    #'   \item{non-existent}{No interpretable strength measure available}
    #' }
    #'
    getStrength = function() {
      return(get_strength_from_test(self))
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
      cat("Results: \n")

      # Table output for multiple p-values to get better readability
      p_vals     <- self$getResult()$p.value
      strengths  <- self$getStrength()[[1]]
      sig_flags  <- self$isSignificant()

      if (length(p_vals) > 1) {

        colored_sig <- sapply(sig_flags, function(val) {
          if (isTRUE(val)) "\033[32mTRUE\033[0m" else "\033[31mFALSE\033[0m"
        })

        # CHATGPT ----------
        cat(sprintf("  %-25s %-12s %-25s %s\n", "Name", "p.value", paste("Strength (", names(self$getStrength()), ")"), "Significant"))
        for (i in seq_along(p_vals)) {
          name <- names(p_vals)[i]
          if (is.null(name) || name == "") name <- paste0("Var", i)

          cat(sprintf(
            "  %-25s %-12.4g %-25.4g %s\n",
            name,
            p_vals[i],
            strengths[i],
            colored_sig[i]
          ))
        }
        # -----------------

      } else {
        cat("  p.value: ", p_vals, "\n")
        cat("  Strength: ", names(self$getStrength())[[1]], "=", round(strengths, 3), "\n")
        sig_colored <- if (isTRUE(sig_flags)) "\033[32mTRUE\033[0m" else "\033[31mFALSE\033[0m"
        cat("  Significant: ", sig_colored, "\n")
      }
    }
  )
)
