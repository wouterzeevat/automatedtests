#' Check if a dataframe is parametric (Internal Function)
#'inst
#'
#' @param data The data to check (vector of integers).
#' @return TRUE if data is normalized, FALSE otherwise. If data is not numeric
#' the function will return NULL
#' @keywords internal
check_parametric <- function(data) {
  if (!is.numeric(data)) return(NULL)

  if (length(data) < 5000) {
    result <- shapiro.test(data)
    test <- "Shapiro-Wilk Test"
  } else {
    result <- ad.test(data)
    test <- "Anderson-Darling Test"
  }

  return(list(
    test = test,
    statistic = result$statistic,
    p_value = result$p.value,
    result = result$p.value > 0.05
  ))
}

#' Returns a statistical test function based on the name
#' of a test. (Internal Function)
#'
#' @importFrom MASS lm
#'
#' @param name The name of a function
#' @param data The data to be used within the statistical test
#' @return The corresponding function as a string, later to be ran.
#' @keywords internal
get_test_from_string <- function(test_object) {
  data <- test_object$getData()
  identifiers <- test_object$getIdentifiers()
  compare_to <- test_object$getCompareTo()
  df <- NULL

  # Format data to fit in tests
  if (length(test_object$getDatatypes()) == 2 && length(unique(test_object$getDatatypes())) > 1) {
    qual_index <- which(test_object$getDatatypes() == "Qualitative")
    quan_index <- which(test_object$getDatatypes() == "Quantitative")

    # For paired test process identifiers
    if (test_object$isPaired()) {
      df <- data.frame(
        id = test_object$getIdentifiers(),
        condition = as.factor(data[[qual_index]]),
        value = data[[quan_index]]
      )
    }
  }
  switch(test_object$getTest(),

         "One-proportion test" = {

           # Makes sure compare_to is correct value, if not use the default
           if (is.null(compare_to) | length(compare_to) > 2) {
             warning("Using default compare_to value this case: ", "0.5 (50%)")
             compare_to <- 0.5 # Uniform distribution
             test_object$setCompareTo(compare_to)
           }
           return(prop.test(sum((data[1] == as.character(data[1][1,]))), nrow(data[1]), compare_to))
         },

         "Chi-square goodness-of-fit test" = {

           # Makes sure compare_to is correct value, if not use the default
           if (is.null(compare_to) | length(compare_to) < 2) {
             warning("Using default compare_to value this case: ",
                     rep(1 / length(unique(data[[1]])), length(unique(data[[1]]))))
             compare_to <- rep(1 / length(unique(data[[1]])), length(unique(data[[1]]))) # Uniform distribution
             test_object$setCompareTo(compare_to)
           }

           counts <- table(data[1])
           return(chisq.test(counts, p = compare_to, rescale.p = TRUE))
         },

         "One-sample Student's t-test" = {

           # Makes sure compare_to is correct value, if not use the default
           if (is.null(compare_to) | length(compare_to) > 2) {
             warning("Using default compare_to value this case: 0")
             compare_to <- 0
             test_object$setCompareTo(compare_to)
           }
           return(t.test(data[1], mu=compare_to))
         },

         "One-sample Wilcoxon test" = {

          # Makes sure compare_to is correct value, if not use the default
          if (is.null(compare_to) | length(compare_to) > 2) {
             warning("Using default compare_to value this case: 0")
             compare_to <- 0
             test_object$setCompareTo(compare_to)
           }
           return(wilcox.test(data[[1]], mu=compare_to))
         },

         "Multiple linear regression" = {
           predictors <- colnames(data[-1])
           formula <- as.formula(paste(colnames(data)[1], " ~", paste(predictors, collapse = " + ")))
           test <- lm(formula, data = data)
           return(test)
         },

         "Binary logistic regression" = {
           predictors <- colnames(data[-1])
           formula <- as.formula(paste(colnames(data)[1], " ~", paste(predictors, collapse = " + ")))
           test <- glm(formula, data = data, family = binomial())
           return(test)
         },

         "Multinomial logistic regression" = {
           predictors <- colnames(data[-1])
           formula <- as.formula(paste(colnames(data)[1], " ~", paste(predictors, collapse = " + ")))
           test <- nnet::multinom(formula, data = data)
           return(test)
         },

         "Pearson correlation" = {
           return(cor.test(data[[1]], data[[2]]))
         },

         "Spearman's rank correlation" = {
           return(suppressWarnings(cor.test(data[[1]], data[[2]], method='spearman')))
         },

         "Cochran's Q test" = {
           tab <- table(data[[1]], data[[2]])
           return(cochran.qtest(tab))
         },

         "McNemar's test" = {
           tab <- table(data[[1]], data[[2]])
           return(mcnemar.test(tab))
         },

         "Fisher's exact test" = {
           tab <- table(data[[1]], data[[2]])
           return(fisher.test(tab))
         },

         "Chi-square test of independence" = {
           tab <- table(data[[1]], data[[2]])
           return(chisq.test(tab))
         },

         "Student's t-test for independent samples" = {
           return(t.test(data[[quan_index]] ~ data[[qual_index]], var.equal = TRUE))
         },

         "Welch's t-test for independent samples" = {
           return(t.test(data[[quan_index]] ~ data[[qual_index]], var.equal = FALSE))
         },

         "Mann-Whitney U test" = {
           return(wilcox.test(data[[quan_index]] ~ data[[qual_index]], exact = FALSE))
         },

         "Student's t-test for paired samples" = {
           return(t.test(
             data[data[[qual_index]] == unique(data[[qual_index]])[1], quan_index],
             data[data[[qual_index]] == unique(data[[qual_index]])[2], quan_index],
             paired = TRUE
           ))
         },

         "Wilcoxon signed-rank test" = {
           return(wilcox.test(
             data[data[[qual_index]] == unique(data[[qual_index]])[1], quan_index],
             data[data[[qual_index]] == unique(data[[qual_index]])[2], quan_index],
             paired = TRUE
           ))
         },

         "One-way ANOVA" = {
           return(aov(data[[quan_index]] ~ data[[qual_index]]))
         },

         "Welch's ANOVA" = {
           return(oneway.test(data[[quan_index]] ~ data[[qual_index]], var.equal = FALSE))
         },

         "Repeated messures ANOVA" = {
           return(aov(value ~ condition + Error(id/condition), data = df))
         },

         "Kruskal-Wallis test" = {
           return(kruskal.test(data[[quan_index]] ~ data[[qual_index]]))
         },

         "Friedman test" = {
           return(friedman.test(value ~ condition | id, data = df))
         },

  )

  stop("An invalid test was chosen.")
}
