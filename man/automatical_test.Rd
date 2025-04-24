\name{automatical_test}
\alias{automatical_test}
\title{Automatically Run a Statistical Test}
\description{
  Automatically choose the best fitting statistical test for your data, and returns an easily readable `AutomatedTest` object from either a data frame or individual vectors. This object contains the executed test together with all statistics and properties.
}
\usage{
  automatical_test(..., compare_to = NULL, identifiers = FALSE)
}
\arguments{
  \item{...}{
    Either a single data frame or multiple equal-length vectors representing columns of data.
  }
  \item{compare_to}{
  A numeric value to compare against during a one-sample test.
  If the data is categorical, the value will default to \eqn{\frac{1}{k}},
  where \eqn{k} is the number of categories, assuming a uniform distribution. If numeric, the default will be 0.
  }
  \item{identifiers}{
    Logical; if TRUE, the first column/vector is treated as identifiers and excluded from testing.
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

  If you want to override the defaults, you can change the \code{compare_to} value to specify one sample tests.

  Once the test has been executed, you can use the method \code{$getResult()} on the resulting object to get more detailed information about the test's execution, including a summary of the test used and all statistics.
}

\examples{
  # Example 1: Using a data frame
  test1 <- automatical_test(iris)

  # Example 2: Using individual vectors
  test2 <- automatical_test(iris$Species, iris$Sepal.Length, identifiers = FALSE)

  # Example output when calling automatical_test with a data vector:
  # Automated Test:
  # Data:  Buy, Age
  # Test:  Spearman's rank correlation
  # Results:
  #   p.value:  3.981442e-05
  #   Significant:  TRUE

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
