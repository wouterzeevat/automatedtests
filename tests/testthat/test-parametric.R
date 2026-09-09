# Normality is a property of each group (or of paired differences), not of the
# pooled response. Two normal groups with different means form a bimodal mixture
# that fails a pooled Shapiro-Wilk test, which used to push automatical_test()
# into a non-parametric test by mistake.

two_normal_groups <- function(n = 50, shift = 6) {
  set.seed(1)
  data.frame(
    value = c(rnorm(n, mean = 0), rnorm(n, mean = shift)),
    group = rep(c("A", "B"), each = n),
    stringsAsFactors = FALSE
  )
}

test_that("normality is checked per group, not on the pooled column", {
  d <- two_normal_groups()

  # Sanity: the pooled column really does fail, the groups do not
  expect_lt(shapiro.test(d$value)$p.value, 0.05)
  expect_gt(shapiro.test(d$value[d$group == "A"])$p.value, 0.05)
  expect_gt(shapiro.test(d$value[d$group == "B"])$p.value, 0.05)

  test <- suppressWarnings(automatical_test(d))
  pl <- test$get_parametric_list()

  expect_equal(nrow(pl), 2)
  expect_setequal(pl$Feature, c("value (group = A)", "value (group = B)"))
  expect_true(all(pl$result))
  expect_true(test$is_parametric())
  expect_match(test$get_test(), "t-test for independent samples")
})

test_that("three normal groups with different means get an ANOVA, not Kruskal-Wallis", {
  set.seed(2)
  d <- data.frame(
    value = c(rnorm(40, 0), rnorm(40, 5), rnorm(40, 10)),
    group = rep(c("A", "B", "C"), each = 40),
    stringsAsFactors = FALSE
  )
  test <- suppressWarnings(automatical_test(d))

  expect_equal(nrow(test$get_parametric_list()), 3)
  expect_true(test$is_parametric())
  expect_match(test$get_test(), "ANOVA")
})

test_that("skewed groups still fall back to a non-parametric test", {
  set.seed(3)
  d <- data.frame(
    value = c(rexp(50, 1), rexp(50, 1) + 6),
    group = rep(c("A", "B"), each = 50),
    stringsAsFactors = FALSE
  )
  test <- suppressWarnings(automatical_test(d))

  expect_false(test$is_parametric())
  expect_equal(test$get_test(), "Mann-Whitney U test")
})

test_that("the group split works when the grouping variable is 0/1 numeric", {
  d <- two_normal_groups()
  d$group <- as.integer(d$group == "B")
  test <- suppressWarnings(automatical_test(d))

  expect_setequal(test$get_parametric_list()$Feature,
                  c("value (group = 0)", "value (group = 1)"))
  expect_true(test$is_parametric())
})

test_that("paired quantitative data tests the differences", {
  set.seed(4)
  before <- rnorm(30, 10)
  after <- before + rnorm(30, 2)
  d <- data.frame(before = before, after = after)
  test <- suppressWarnings(automatical_test(d, paired = TRUE))
  pl <- test$get_parametric_list()

  expect_equal(pl$Feature, "before - after")
  expect_equal(test$get_test(), "Student's t-test for paired samples")
})

test_that("paired group data with two conditions tests the differences", {
  set.seed(5)
  base <- rnorm(30, 10)
  d <- data.frame(
    id = rep(1:30, 2),
    condition = rep(c("pre", "post"), each = 30),
    value = c(base, base + rnorm(30, 2)),
    stringsAsFactors = FALSE
  )
  test <- suppressWarnings(automatical_test(d, identifiers = TRUE))
  pl <- test$get_parametric_list()

  expect_equal(nrow(pl), 1)
  expect_match(pl$Feature, "value \\(.* - .*\\)")
  expect_equal(test$get_test(), "Student's t-test for paired samples")
})

test_that("unpaired quantitative pairs (correlation) test each variable", {
  set.seed(6)
  d <- data.frame(x = rnorm(40), y = rnorm(40))
  test <- suppressWarnings(automatical_test(d))

  expect_setequal(test$get_parametric_list()$Feature, c("x", "y"))
  expect_equal(test$get_test(), "Pearson correlation")
})

test_that("groups too small to test do not error and are ignored", {
  set.seed(7)
  d <- data.frame(
    value = c(rnorm(30), 1.5, 2.5),
    group = c(rep("A", 30), "B", "B"),
    stringsAsFactors = FALSE
  )
  test <- suppressWarnings(automatical_test(d))
  pl <- test$get_parametric_list()

  expect_equal(nrow(pl), 2)
  expect_true(is.na(pl$result[pl$Feature == "value (group = B)"]))
  expect_true(test$is_parametric())
})

test_that("check_parametric handles tiny, constant and non-numeric input", {
  expect_null(check_parametric(c("a", "b", "c")))
  expect_true(is.na(check_parametric(c(1, 2))$result))
  expect_true(is.na(check_parametric(c(3, 3, 3, 3))$result))
  expect_true(is.na(check_parametric(c(1, NA, 2))$result))
  expect_true(check_parametric(c(1.1, 2.3, 2.9, 4.2, 5.0))$result)
})
