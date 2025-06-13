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
    .set_test = function(test) {
      stopifnot(is.character(test))
      private$.test <- test
    },

    .set_result = function(result) {
      private$.result <- result
    },

    .set_parametric_list = function(parametric) {
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

      private$.set_test(pick_test(test_object = self))
      private$.set_result(get_test_from_string(test_object = self))
    },

    #' @description Get the data used in the test
    #' @return A dataframe with all features
    get_data = function() {
      return(private$.data)
    },

    #' @description Shows if the data is paired, if there are multiple rows with the same identifier, the data has more
    #' samples (TIDY DATA). Making the data paired.
    #'
    #' @return Whether the data is paired (TRUE/FALSE).
    is_paired = function() {
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
    get_identifiers = function() {
      return(private$.identifiers)
    },

    #' @description Get the comparison value for one-sample tests
    #' @return A numeric value for comparison
    get_compare_to = function() {
      return(private$.compare_to)
    },

    #' @description Updates the compare_to variable. Is public because the
    #' compare value can get changed depending on the type of test. This
    #' function is public because it needs to be able to be called by
    #' automatical_test()
    #' @param compare_to Numeric value to compare to.
    #' @return Updated object with comparison value set.
    set_compare_co = function(compare_to) {
      private$.compare_to <- compare_to
    },

    #' @description Get the data types of the features in the object
    #' @return A list of data types (e.g., Quantitative or Qualitative)
    get_datatypes = function() {
      result <- c()
      for (feature in names(self$get_data())) {
        if (is.numeric(self$get_data()[[feature]]) && !all((unique(self$get_data()[[feature]]) %in% c(0,1)))) {
          result <- append(result, "Quantitative")
        } else {
          result <- append(result, "Qualitative")
        }
      }
      return(result)
    },

    #' @description Get the parametric test results of the features
    #' @return A list of parametric test results
    get_parametric_list = function() {

      parametric_list <- data.frame(
        Feature = character(),
        result = logical(),
        p_value = numeric(),
        test = logical(),
        statistic = numeric(),
        stringsAsFactors = FALSE
      )

      for (name in colnames(self$get_data())) {
        feature = self$get_data()[[name]]
        result = check_parametric(feature)

        # Ignore qualitative
        if (is.null(result)) {
          next
        }

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
    is_parametric = function() {
      return(all(self$get_parametric_list()$result))
    },

    #' @description Get the statistical test that was chosen
    #' @return The name of the statistical test
    get_test = function() {
      return(private$.test)
    },

    #' @description Get the result of selected statistical test
    #' @return The result of the statistical test
    get_result = function() {
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
    get_strength = function() {
      return(get_strength_from_test(self))
    },

    #' @description Whether the test results are significant or not.
    #' @return TRUE / FALSE depending on the significance of the test.
    is_significant = function() {
      return(self$get_result()$p.value < 0.05)
    },

    #' @description Print a summary of the test object
    print = function() {
      cat("Automated Test:\n")
      cat("Data: ", paste0(colnames(self$get_data()), collapse = ", "), "\n")

      # If one sample test
      size <- ncol(self$get_data())
      if (size == 1) {
        cat("Compared to: ", self$get_compare_to(), "\n")
      }

      cat("Test: ", self$get_test(), "\n")
      cat("Results: \n")

      # Table output for multiple p-values to get better readability
      p_vals     <- self$get_result()$p.value
      strengths  <- self$get_strength()[[1]]
      sig_flags  <- self$is_significant()

      if (length(p_vals) > 1) {

        colored_sig <- sapply(sig_flags, function(val) {
          if (isTRUE(val)) "\033[32mTRUE\033[0m" else "\033[31mFALSE\033[0m"
        })

        # CHATGPT ----------
        cat(sprintf("  %-25s %-12s %-25s %s\n", "Name", "p.value", paste("Strength (", names(self$get_strength()), ")"), "Significant"))
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
        cat("  Strength: ", names(self$get_strength())[[1]], "=", round(strengths, 3), "\n")
        sig_colored <- if (isTRUE(sig_flags)) "\033[32mTRUE\033[0m" else "\033[31mFALSE\033[0m"
        cat("  Significant: ", sig_colored, "\n")
      }
    }
  )
)
