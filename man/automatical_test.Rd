\name{automatical_test}
\alias{automatical_test}
\title{Automatically Run a Statistical Test}
\description{
  This function creates an `AutomatedTest` object from either a data frame or individual vectors, automating the process of selecting statistical tests.
}
\usage{
  automatical_test(..., compare_to = NULL, identifiers = FALSE)
}
\arguments{
  \item{...}{
    Either a single data frame or multiple equal-length vectors representing columns of data.
  }
  \item{compare_to}{
    A numeric value to compare against during a ONE SAMPLE TEST.
    If data is categorical, the value will be set to \( \frac{1}{\text{column size}} \) by default (assuming uniform data).
    If data is numeric, the default value will be 0 (comparing to 0).
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
  If \code{identifiers} is set to TRUE, the first column will be used as identifiers, supporting TIDY data.

  The \code{compare_to} parameter is dynamically set to different values based on the data type:
  - For **categorical data**, the default value is \( \frac{1}{\text{column size}} \).
  - For **numeric data**, the default value is 0.

  If you want to override the defaults, you can change the \code{compare_to} value.

  Once the test has been executed, you can use the method \code{$getResult()} on the resulting object to get more detailed information about the test's execution, it will list the entire executed test.
}
\examples{
  # Example 1: Using a data frame
  test1 <- automatical_test(x)

  # Example 2: Using individual vectors
  test2 <- automatical_test(x[[1]], x[[2]], x[[3]], identifiers = TRUE)

  # Example output when calling automatical_test with a data vector:
  # Automated Test:
  # Data:  Buy Age
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
