#' Automatical test function
#'
#' @description The
#' @export
automatical_test <- function(data=NULL, subsets=NULL, compare_to=0.5, identifiers=FALSE) {

  # Split identifiers from data if enabled
  ids <- list()
  if (identifiers) {
    ids <- data[[1]]
    data <- data[, -1]
  }

  if (is.null(data)) {
    stop("Data argument is not a valid dataframe!")
  }

  if (!is.null(subsets) && !(dim(data)[1] == length(subsets))) {
    stop("Subsets are defined but do not match the size of the data!")
  }

  test <- AutomatedTest$new(data, subsets, ids, compare_to)
  return(test)

}

x <- read.csv("diabetes.csv")
subset_gender <- sample(c("Male", "Female"), size = nrow(x), replace = TRUE)

