#' Automatically Run a Statistical Test
#'
#' This function creates an `AutomatedTest` object from either a data frame or individual vectors, automating the process of selecting statistical tests.
#'
#' @param ... Either a single data frame or multiple equal-length vectors representing columns of data.
#' @param compare_to A numeric value to compare against during a ONE SAMPLE TEST.
#' If data is categorical, the value will be 1 / column size by default (assuming uniform data).
#' When numerical, the value will be 0 by default (comparing to 0)
#' @param identifiers Logical; if TRUE, the first column/vector is treated as identifiers and excluded from testing.
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
automatical_test <- function(..., compare_to = NULL, identifiers = FALSE) {

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
    return(AutomatedTest$new(data, ids))
  }
  return(AutomatedTest$new(data, ids, compare_to))
}
