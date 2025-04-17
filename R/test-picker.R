#' Check if a dataframe is parametric (Internal Function)
#'inst
#'
#' @param data The data to check (vector of integers).
#' @return TRUE if data is normalized, FALSE otherwise.
#' @keywords internal
pick_test <- function(test_object) {
  size <- ncol(test_object$getData())
  if(!is.null(test_object$getSubsets())) {
    size <- size + 1
  }

  if (size == 1) {
    return(pick_one_variable_test(test_object))
  }

  if (size == 2) {
    return(pick_two_variable_test(test_object))
  }
  return("INVALID")
}

pick_one_variable_test <- function(test_object) {
  stopifnot(!is.null(test_object$getCompareTo()) || !is.numeric(test_object$getCompareTo()))

  # Qualitative
  if (test_object$getDatatypes()[1] == "Qualitative") {
    group_size <- length(unique(test_object$getData()[[1]]))
    stopifnot(group_size > 1)
    if (length(unique(test_object$getData()[[1]])) < 3) {
      return("One-proportion test")
    }
    return("Chi-square goodness-of-fit test")
  }

  # Quantitative
  if (test_object$isParametric()) {
    return("One-sample Student's t-test")
  }

  return("One-sample Wilcoxon test")
}

pick_two_variable_test <- function(test_object) {
  types <- test_object$getDatatypes()
  data <- test_object$getData()

  # Quantitative & Quantitative
  if (types[1] == "Quantitative" && types[2] == "Quantitative") {
    if (test_object$isParametric()) {
      return("Pearson correlation")
    }
    return("Spearman's rank correlation")
  }

  # Qualitative & Qualitative
  if (types[1] == "Qualitative" && types[2] == "Qualitative") {

    # USING COCHRAN for paired instead
    #if (length(unique(data[1])) > 2 || length(unique(data[2])) > 2) {
    #  return("Chi-square test of independence")
    #}

    if (test_object$hasIdentifiers()) {
      if (length(unique(data[1])) > 2 || length(unique(data[2])) > 2) {
        return("Cochran's Q test")
      }
      return("McNemar's test")
    }
    table_data <- table(data[[1]], data[[2]])
    chi_result <- chisq.test(table_data)

    # If frequency >= 5
    if (all(chi_result$expected >= 5)) {
      return("Chi-square test of independence")
    }
    return("Fisher's exact test")

  }

  qual_index <- which(types == "Qualitative")
  quan_index <- which(types == "Quantitative")

  # Qualitative & Quantitative
  if (length(unique(data[[qual_index]])) > 2) {
    if (test_object$hasIdentifiers()) {
      if (test_object$isParametric()) {
        return("Repeated messures ANOVA")
      }
      return("Friedman test")
    }
    if (test_object$isParametric()) {

      # Equal variance test
      if (bartlett.test(data[[quan_index]], data[[qual_index]])$p.value > 0.05) {
        return("One-way ANOVA")
      }
      return("Welch's ANOVA")
    }
    return("Kruskal-Wallis test")
  }

}
