#' Automatically Run a Statistical Test
#'
#' This function creates an `AutomatedTest` object from either a data frame or individual vectors, automating the process of selecting statistical tests.
#'
#' @param ... Either a single data frame or multiple equal-length vectors representing columns of data.
#' @param compare_to A numeric value to compare against during a ONE SAMPLE TEST.
#' If data is categorical, the value will be 1 / column size by default (assuming uniform data).
#' When numerical, the value will be 0 by default (comparing to 0)
#' @param identifiers Logical; if TRUE, the first column/vector is treated as identifiers and excluded from testing. This will check if the test has to be paired
#' @param paired Logical; if TRUE, the test will be paired no matter the identifiers are (for mcnemar and cochran test that do not need identifiers specifically)

#'
#' @return An object of class `AutomatedTest`.
#' @export
#'
#' @examples
#' test1 <- automatical_test(x)
#' test2 <- automatical_test(x[[1]], x[[2]], x[[3]], identifiers = TRUE)
#'
#' # Example output when calling automatical_test with a data vector:
#' # Automated Test:
#' # Data:  Buy Age
#' # Test:  Spearman's rank correlation
#' # Results:
#' #   p.value:  3.981442e-05
#' #   Significant:  TRUE
automatical_test <- function(..., compare_to = NULL, identifiers = FALSE, paired = FALSE) {

  args <- list(...)

  if (length(args) == 1 && is.data.frame(args[[1]])) {
    data <- args[[1]]
  } else {
    data <- tryCatch(as.data.frame(args), error = function(e) stop("Invalid data, is the data correctly formatted?"))

    lengths <- sapply(data, length)
    if (length(unique(lengths)) != 1) {
      stop("All input vectors must be the same length!")
    }
  }

  # Split identifiers from data if enabled
  ids <- list()
  if (identifiers) {
    ids <- data[[1]]
    data <- data[, -1, drop = FALSE]
  }

  # Create and return the AutomatedTest object
  if (is.null(compare_to)) {
    test <- AutomatedTest$new(data, ids, paired=paired)
  }
  test <- AutomatedTest$new(data, ids, compare_to, paired=paired)

  # Gives a warning because the parametric check could be too sensitive. Manual inspection is recommended!
  if (!test$is_parametric()) {
    warning("Normality test suggests the data may not be normally distributed.
    It is recommended to visually inspect the data using a Q-Q plot.
    If the data is in fact parametric but contains outliers,
    consider manually applying normalization or transformation before proceeding", immediate.=TRUE)
  }


  # Warning for correlations, only supports linear relationships right now!
  if (grepl("correlation", test$get_test())) {
    warning("AutomatedTest correlation currently detects only linear relationships.
  Please check the data pattern using plot() to ensure the test is appropriate.")
  }

  return(test)
}
