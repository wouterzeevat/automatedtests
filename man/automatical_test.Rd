\name{automatical_test}
\alias{automatical_test}
\title{Automatically Run a Statistical Test}
\description{
  Automatically choose the best fitting statistical test for your data, and returns an easily readable \code{AutomatedTest} object from either a data frame or individual vectors. This object contains the executed test together with all statistics and properties.
}
\usage{
  automatical_test(..., compare_to = NULL, identifiers = FALSE, paired = FALSE)
}
\arguments{
  \item{...}{
    Either a single data frame or multiple equal-length vectors representing columns of data.
  }
  \item{compare_to}{
    A numeric value to compare against during a one-sample test.
    If the data is categorical, the value will default to \eqn{1/k},
    where \eqn{k} is the number of categories, assuming a uniform distribution.
    If numeric, the default will be 0.
  }
  \item{identifiers}{
    Logical; if TRUE, the first column/vector is treated as identifiers and excluded from testing.
  }
  \item{paired}{
    Logical; if TRUE, the test will be performed as paired if applicable, regardless of whether identifiers are provided.
    This applies to paired tests like McNemar's or the Cochran Q test.
  }
}
\value{
  An object of class \code{AutomatedTest}.
  The object contains the results of the statistical test performed on the data.
  You can use the method \code{$getResult()} to obtain more detailed information about the execution of the test.
}
\details{
  The \code{automatical_test} function automatically selects and runs the most fitting statistical test based on the data provided.
  It can accept data as either a single data frame or multiple individual vectors, provided the vectors are of equal length.

  If \code{identifiers} is set to TRUE, the first column will be treated as identifiers and excluded from the test, supporting TIDY data.

  When a multiple group test is selected (i.e., more than two groups, columns, or variables are used),
  the first non-identifier column will be used as the grouping or target variable, meaning all other variables will be tested against it.

  The \code{paired} parameter can be used to force paired testing for supported tests (such as McNemar's test or Cochran's Q),
  even if identifiers are not explicitly included in the input.

  If you want to override the defaults, you can change the \code{compare_to} value to specify one-sample tests.

  Once the test has been executed, you can use the method \code{$getResult()} on the resulting object to get more detailed information about the test's execution, including a summary of the test used and all statistics.

  Supported tests:
  \tabular{rl}{
  \strong{ID} \tab \strong{Test} \cr
  1  \tab One-proportion test \cr
  2  \tab Chi-square goodness-of-fit test \cr
  3  \tab One-sample Student's t-test \cr
  4  \tab One-sample Wilcoxon test \cr
  5  \tab Multiple linear regression \cr
  6  \tab Binary logistic regression \cr
  7  \tab Multinomial logistic regression \cr
  8  \tab Pearson correlation \cr
  9  \tab Spearman's rank correlation \cr
  10 \tab Cochran's Q test \cr
  11 \tab McNemar's test \cr
  12 \tab Fisher's exact test \cr
  13 \tab Chi-square test of independence \cr
  14 \tab Student's t-test for independent samples \cr
  15 \tab Welch's t-test for independent samples \cr
  16 \tab Mann-Whitney U test \cr
  17 \tab Student's \cr
  }
}
\examples{
  # Example 1: Using individual vectors
  test1 <- automatical_test(iris$Species, iris$Sepal.Length, identifiers = FALSE)

  # Example 2: Forcing a paired test
  before <- c(200, 220, 215, 205, 210)
  after <- c(202, 225, 220, 210, 215)
  paired_data <- data.frame(before, after)
  test2 <- automatical_test(before, after, paired = TRUE)

  # Retrieve more detailed information about the test
  # test1$getResult()
}
\author{
  Wouter Zeevat
}
\seealso{
  \code{\link{AutomatedTest}} for the class used by this function.
}
\keyword{automated statistical test}
