#' Check if a dataframe is parametric (Internal Function)
#'inst
#'
#' @param data The data to check (vector of integers).
#' @return TRUE if data is normalized, FALSE otherwise.
#' @keywords internal
check_parametric <- function(data) {
  if (!is.numeric(data)) return(FALSE)

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
  subsets <- test_object$getSubsets()
  compare_to <- test_object$getCompareTo()
  switch(test_object$getTest(),

         "One-proportion test" = {

           # Makes sure compare_to is correct value, if not use the default
           if (length(compare_to) > 2) {
             warning("compare_to is an invalid value: ", compare_to, ". changed to default for this case: ", "0.5 (50%)")
             compare_to <- 0.5 # Uniform distribution
             test_object$setCompareTo(compare_to)
           }
           return(prop.test(sum((data[1] == as.character(data[1][1,]))), nrow(data[1]), compare_to))
         },

         "Chi-square goodness-of-fit test" = {

           # Makes sure compare_to is correct value, if not use the default
           if (length(compare_to) < 2) {
             warning("compare_to is an invalid value: ", compare_to, ". changed to default for this case: ",
                     rep(1 / length(unique(data[[1]])), length(unique(data[[1]]))))
             compare_to <- rep(1 / length(unique(data[[1]])), length(unique(data[[1]]))) # Uniform distribution
             test_object$setCompareTo(compare_to)
           }

           counts <- table(data[1])
           return(chisq.test(counts, p = compare_to, rescale.p = TRUE))
         },

         "One-sample Student's t-test" = {

           # Makes sure compare_to is correct value, if not use the default
           if (length(compare_to) > 2) {
             warning("compare_to is an invalid value: ", compare_to, ". changed to default for this case: 0")
             compare_to <- 0
             test_object$setCompareTo(compare_to)
           }
           return(t.test(data[1], mu=compare_to))
         },

         "One-sample Wilcoxon test" = {

          # Makes sure compare_to is correct value, if not use the default
          if (length(compare_to) > 2) {
             warning("compare_to is an invalid value: ", compare_to, ". changed to default for this case: 0")
             compare_to <- 0
             test_object$setCompareTo(compare_to)
           }
           return(wilcox.test(data[[1]], mu=compare_to))
         },

         "Multiple linear regression" = {
           lm(values ~ group, data = data)
         },

         "Binary logistic regression" = {
           glm(group ~ values, family = binomial, data = data)
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
           t.test(values ~ group, data = data, var.equal=TRUE)
         },

         "Welch's t-test for independent samples" = {
           t.test(values ~ group, data = data, var.equal=FALSE)
         },

         "Mann-Whitney U test" = {
           wilcox.test(values ~ group, data = data)
         },

         "Student's t-test for paired samples" = {
           t.test(data$values[1:10], data$values[11:20], paired=TRUE)
         },

         "Wilcoxon signed-rank test" = {
           wilcox.test(data$values[1:10], data$values[11:20], paired=TRUE)
         },

         "One-way ANOVA" = {
           return(aov(data[[1]], data[[2]]))
         },

         "Welch's ANOVA" = {
           oneway.test(values ~ group, data = data)
         },

         "Repeated messures ANOVA" = {
           oneway.test(values ~ group, data = data)
         },

         "Kruskal-Wallis test" = {
           kruskal.test(values ~ group, data = data)
         },

         "Friedman test" = {
           return(friedman.test(data[[1]] ~ data[[2]] | factor(rep(1:nrow(data), each=2))))
         },

  )

  stop("An invalid test was chosen.")
}
