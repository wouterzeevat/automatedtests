#' Check if a dataframe is parametric (Internal Function)
#'inst
#'
#' @param test_object The data to check (vector of integers).
#' @return TRUE if data is normalized, FALSE otherwise.
#' @keywords internal
pick_test <- function(test_object) {
  size <- ncol(test_object$get_data())

  if (size == 1) {
    return(pick_one_variable_test(test_object))
  }

  if (size == 2) {
    return(pick_two_variable_test(test_object))
  }
  return(pick_multiple_variable_test(test_object))
}

#' Pick the appropriate test for one variable (Internal Function)
#'
#' @param test_object An object containing the data, data types, and comparison value.
#' @return A character string with the name of the appropriate one-sample statistical test.
#' @keywords internal
pick_one_variable_test <- function(test_object) {
  stopifnot(!is.null(test_object$get_compare_to()) || !is.numeric(test_object$get_compare_to()))

  # Qualitative
  if (test_object$get_datatypes()[1] == "Qualitative") {
    group_size <- length(unique(test_object$get_data()[[1]]))
    stopifnot(group_size > 1)
    if (length(unique(test_object$get_data()[[1]])) < 3) {
      return("One-proportion test")
    }
    return("Chi-square goodness-of-fit test")
  }

  # Quantitative
  if (test_object$is_parametric()) {
    return("One-sample Student's t-test")
  }

  return("One-sample Wilcoxon test")
}

#' Pick the appropriate test for two variables (Internal Function)
#'
#' @param test_object An object containing the data, types, and metadata needed for test selection.
#' @return A character string with the name of the appropriate statistical test.
#' @keywords internal
pick_two_variable_test <- function(test_object) {
  types <- test_object$get_datatypes()
  data <- test_object$get_data()

  # Quantitative & Quantitative
  if (types[1] == "Quantitative" && types[2] == "Quantitative") {

    # Paired test
    if (test_object$is_paired()) {
      if (test_object$is_parametric()) {
        return("Student's t-test for paired samples")
      } else {
        return("Wilcoxon signed-rank test")
      }
    }

    # Correlation
    if (test_object$is_parametric()) {
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

    if (test_object$is_paired()) {
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
    if (test_object$is_paired()) {
      if (test_object$is_parametric()) {
        return("Repeated measures ANOVA")
      }
      return("Friedman test")
    }
    if (test_object$is_parametric()) {

      # Equal variance test
      if (bartlett.test(data[[quan_index]], data[[qual_index]])$p.value > 0.05) {
        return("One-way ANOVA")
      }
      return("Welch's ANOVA")
    }
    return("Kruskal-Wallis test")
  }

  # Qualtitative Group size == 2
  if (test_object$is_paired()) {
    if (test_object$is_parametric()) {
      return("Student's t-test for paired samples")
    }
    return("Wilcoxon signed-rank test")
  }

  # Qualitative Group size > 2
  if (test_object$is_parametric()) {

    # Equal variance test
    if (bartlett.test(data[[quan_index]], data[[qual_index]])$p.value > 0.05) {
      return("Student's t-test for independent samples")
    }
    return("Welch's t-test for independent samples")
  }
  return("Mann-Whitney U test")

}

#' Pick the appropriate test for multiple variables (Internal Function)
#'
#' @param test_object An object containing the data, types, and metadata needed for test selection.
#' @return A character string with the name of the appropriate regression or classification model.
#' @keywords internal
pick_multiple_variable_test <- function(test_object) {
  types <- test_object$get_datatypes()
  data <- test_object$get_data()

  # If binary / 2 groups
  if (length(unique(data[[1]])) == 2 && all(unique(data[[1]]) %in% c(0, 1, TRUE, FALSE)) ||
      !is.numeric(data[[1]]) && length(unique(data[[1]])) == 2) {
    return("Binary logistic regression")
  }

  if (is.numeric(data[[1]])) {
    return("Multiple linear regression")
  }

  return("Multinomial logistic regression")

}
