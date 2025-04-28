# automaticalTest

Automatically select and run the best statistical test for your data with just one line of code. Supporting one-sample-tests, two-sample-tests, multiple-sample-tests, and even correlations!

## What is `automaticedtest`?

`automatedtests` is an R package designed to simplify statistical testing. It automatically analyzes your data, determines the most fitting statistical test (based on structure and content), and executes it. shortening the time spent deciding what test to use.

The package supports a tidy data frames and a set of numeric/categorical vectors!

## Features

- Auto-detects best statistical test based on your data type and structure.
- Handles tidy data: optional identifier exclusion.
- Returns an `AutomatedTest` object with full test results and a `$getResult()` method for summaries.

### Supported Tests
| number     | test |
|--------------|-------------|
| 1 | One-proportion test
| 2 | Chi-square goodness-of-fit test
| 3 | One-sample Student's t-test
| 4 | One-sample Wilcoxon test
| 5 | Multiple linear regression
| 6 | Binary logistic regression
| 7 | Multinomial logistic regression
| 8 | Pearson correlation
| 9 | Spearman's rank correlation
| 10 | Cochran's Q test
| 11 | McNemar's test
| 12 | Fisher's exact test
| 13 | Chi-square test of independence
| 14 | Student's t-test for independent samples
| 15 | Welch's t-test for independent samples
| 16 | Mann-Whitney U test
| 17 | Student's t-test for paired samples
| 18 | Wilcoxon signed-rank test
| 19 | One-way ANOVA
| 20 | Welch's ANOVA
| 21 | Repeated measures ANOVA
| 22 | Kruskal-Wallis test
| 23 | Friedman test

## Installation

You can install the package from GitHub:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install automaticalTest from GitHub
devtools::install_github("wouterzeevat/automatedtests")
```

## Usage

### Using a data frame

```r
# Automatically runs appropriate test(s) on the iris dataset
test1 <- automatical_test(iris)

# Get detailed results
test1$getResult()
```

### Using individual vectors

```r
# Compare Sepal.Length across Species
test2 <- automatical_test(iris$Species, iris$Sepal.Length)
test2$getResult()
```

### One-sample tests

```r
# Compare a numeric vector to a fixed value
automatical_test(c(3, 5, 4, 6, 7), compare_to = 5)
```

## Arguments

| Argument     | Description |
|--------------|-------------|
| `...`        | A data frame or multiple equal-length vectors |
| `compare_to` | Value to compare against in one-sample tests (numeric or assumed uniform for categorical data) |
| `identifiers`| Logical; if TRUE, the first column is treated as identifiers and excluded from testing |

## Output

Returns an object of class `AutomatedTest` with methods and properties like:

- `$getResult()` — detailed summary of the test performed, containing all information including p.valuem, statistics etc.
- `$getTest()` — test type selected
- `$isParametric()` — p-value('s) of the result
- `$isSignificant()` — TRUE/FALSE if result is statistically significant, to show the result in the blink of an eye!

## Example Output

```r
# Automated Test:
# Data:  Buy, Age
# Test:  Spearman's rank correlation
# Results:
#   p.value:  3.98e-05
#   Significant:  TRUE
```

## Dependencies

- R6
- MASS
- nnet

These are automatically handled during installation.

## Author

Wouter Zeevat

## See Also

- `AutomatedTest` – the class used to store test results.

## License

MIT License
