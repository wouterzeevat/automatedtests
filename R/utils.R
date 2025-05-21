#' Internal: Check if a numeric vector follows a normal distribution
#'
#' This function checks whether a numeric vector is approximately normally distributed,
#' using the Shapiro-Wilk test for small samples (n < 5000) and the Anderson-Darling test
#' for larger ones. If the input is not numeric, the function returns \code{NULL}.
#'
#' @param data A numeric vector to test for normality.
#'
#' @return A list containing:
#' \describe{
#'   \item{test}{Name of the test used ("Shapiro-Wilk Test" or "Anderson-Darling Test")}
#'   \item{statistic}{The test statistic}
#'   \item{p_value}{The p-value from the test}
#'   \item{result}{Logical; \code{TRUE} if p > 0.05 (assumed normal), \code{FALSE} otherwise}
#' }
#'
#' Returns \code{NULL} if input is not numeric.
#'
#' @keywords internal
#'
#' @importFrom stats shapiro.test
#' @importFrom nortest ad.test
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

#' Internal: Returns the result of a statistical test based on a string identifier
#'
#' This internal function selects and runs a statistical test using data from a test object,
#' based on the name of the test specified. It supports a wide variety of tests including
#' t-tests, chi-square tests, ANOVA, correlation tests, regression models, and more.
#'
#' @param test_object An object containing data, identifiers, datatypes, and test selection.
#'
#' @return The result of the selected statistical test. Typically, this is a test object with
#'         class `htest`, `aov`, `lm`, or similar.
#'
#' @keywords internal
#'
#' @importFrom stats prop.test chisq.test t.test wilcox.test as.formula lm coef glm binomial
#' @importFrom stats pnorm cor.test mcnemar.test fisher.test aov oneway.test kruskal.test
#' @importFrom stats friedman.test shapiro.test bartlett.test
#' @importFrom nortest ad.test
#' @importFrom DescTools CochranQTest
#' @importFrom nnet multinom
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

           model_summary <- summary(test)
           test$p.value <- coef(model_summary)[-1, "Pr(>|t|)"]
           return(test)
         },

         "Binary logistic regression" = {
           predictors <- colnames(data[-1])
           formula <- as.formula(paste(colnames(data)[1], " ~", paste(predictors, collapse = " + ")))
           test <- glm(formula, data = data, family = binomial())

           model_summary <- summary(test)
           test$p.value <- coef(model_summary)[-1, "Pr(>|z|)"]
           return(test)
         },

         "Multinomial logistic regression" = {
           predictors <- colnames(data[-1])
           formula <- as.formula(paste(colnames(data)[1], " ~", paste(predictors, collapse = " + ")))
           test <- nnet::multinom(formula, data = data)

           model_summary <- summary(test)

           # CHATGPT
           z_values <- model_summary$coefficients / model_summary$standard.errors
           p_values_matrix <- 2 * (1 - pnorm(abs(z_values)))

           # Aggregate p-values across all outcome levels per predictor
           # For example, take the max (most conservative)
           predictor_pvals <- apply(p_values_matrix, 2, max)

           # Convert to named list
           test$p.value <- predictor_pvals
           return(test)
         },

         "Pearson correlation" = {
           return(cor.test(data[[1]], data[[2]]))
         },

         "Spearman's rank correlation" = {
           return(suppressWarnings(cor.test(data[[1]], data[[2]], method='spearman')))
         },

         "Cochran's Q test" = {
           wide_data <- reshape(data, timevar = names(data)[2], idvar = names(data)[1], direction = "wide")

           # Drop the ID column
           mat <- as.matrix(wide_data[ , -1])

           # Perform Cochran's Q test using DescTools
           return(DescTools::CochranQTest(mat))
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
           if ("Qualitative" %in% test_object$getDatatypes()) {
             data[1] <- data[data[[qual_index]] == unique(data[[qual_index]])[1], quan_index]
             data[2] <- data[data[[qual_index]] == unique(data[[qual_index]])[2], quan_index]
           }
           return(t.test(data[[1]], data[[2]], paired = TRUE))
         },

         "Wilcoxon signed-rank test" = {
           if ("Qualitative" %in% test_object$getDatatypes()) {
             data[1] <- data[data[[qual_index]] == unique(data[[qual_index]])[1], quan_index]
             data[2] <- data[data[[qual_index]] == unique(data[[qual_index]])[2], quan_index]
           }
           return(wilcox.test(data[[1]], data[[2]], paired = TRUE))
         },

         "One-way ANOVA" = {
           test <- aov(data[[quan_index]] ~ data[[qual_index]])
           test$p.value <- summary(test)[[1]]$`Pr(>F)`[1]
           return(test)
         },

         "Welch's ANOVA" = {
           return(oneway.test(data[[quan_index]] ~ data[[qual_index]], var.equal = FALSE))
         },

         "Repeated measures ANOVA" = {

           test <- aov(value ~ condition + Error(id/condition), data = df)
           test$p.value <- summary(test)[[3]][[1]]$`Pr(>F)`[[1]] # P value
           return(test)
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

#' Returns the strength of a test.
#' This is a different kind of value for each test. It will also return what the value is.
#' These are the different types of data it can return:
#'
#' This function takes a `test_object` that contains the result of a statistical test
#' and returns the main coefficient, estimate, or test statistic as a numeric value.
#' It supports various tests such as t-tests, ANOVAs, regressions, and correlations.
#'
#' @param test_object An object containing a statistical test result and metadata,
#'        expected to have methods `getResult()` and `getTest()`.
#'
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
#' @examples
#' # Assuming test_object is set up:
#' get_strength_from_test(test_object)
#'
#' @keywords internal
get_strength_from_test <- function(test_object) {
  result <- test_object$getResult()

  switch(test_object$getTest(),

         # Regressions — keep full coefficient vector with original names
         "Multiple linear regression" = ,
         "Binary logistic regression" = {
           coefs <- coef(result)[-1]
           return(list(coefficient = coefs))
         },

         "Multinomial logistic regression" = {
           coefs <- result$coefficients
           return(list(coefficient = coefs))
         },

         # Correlations — return r with fixed name
         "Pearson correlation" = ,
         "Spearman's rank correlation" = {
           return(setNames(as.numeric(result$estimate), "r"))
         },

         # T-tests — return estimated mean diff or means
         "One-sample Student's t-test" = ,
         "Student's t-test for paired samples" = ,
         "Student's t-test for independent samples" = ,
         "Welch's t-test for independent samples" = {
           est <- result$estimate
           return(setNames(as.numeric(est), "mean difference"))
         },

         # Non-parametric — return main test statistic
         "One-sample Wilcoxon test" = ,
         "Wilcoxon signed-rank test" = ,
         "Mann-Whitney U test" = ,
         "Chi-square test of independence" = ,
         "Chi-square goodness-of-fit test" = ,
         "Cochran's Q test" = ,
         "McNemar's test" = {
           return(setNames(as.numeric(result$statistic[[1]]), "statistic"))
         },

         # ANOVA-style — extract F statistic
         "One-way ANOVA" = ,
         "Welch's ANOVA" = ,
         "Repeated measures ANOVA" = ,
         "Kruskal-Wallis test" = ,
         "Friedman test" = {
           if (inherits(result, "aov")) {
             f_val <- summary(result)[[1]]$`F value`[1]
             return(setNames(as.numeric(f_val), "F statistic"))
           } else {
             return(setNames(as.numeric(result$statistic[[1]]), "F statistic"))
           }
         },

         # Proportion test — fixed label
         "One-proportion test" = {
           return(setNames(as.numeric(result$estimate[[1]]), "proportion"))
         },

         # Unknown or unsupported test
         {
           return(setNames(NA, "non-existent"))
         }
  )
}


