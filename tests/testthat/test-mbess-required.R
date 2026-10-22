library(apaTables)

# MBESS computes the confidence intervals for d values, R-squared and
# partial eta-squared. As a suggested package it was not installed by
# install.packages(), and without it the regression and ANOVA tables printed
# [NA, NA], and a d-value table written by apa.save() or Quarto printed
# [-99.00, -99.00] as each interval under a note describing 95% confidence
# intervals (the console view dropped the interval rows instead). It is a
# required import so that every installation prints the intervals; these
# tests fail if it is moved back.

test_that("MBESS is a required import, not a suggested package", {
  imports <- utils::packageDescription("apaTables", fields = "Imports")
  suggests <- utils::packageDescription("apaTables", fields = "Suggests")
  pkgs <- function(field) trimws(sub("\\(.*", "", strsplit(field, ",")[[1]]))
  expect_true("MBESS" %in% pkgs(imports))
  expect_false("MBESS" %in% pkgs(suggests))
})

test_that("the d, R-squared and partial eta-squared intervals are numbers", {
  r2 <- apa.reg.table(lm(mpg ~ wt, data = mtcars))$table_body
  fit <- r2[, "Fit"]
  expect_false(any(grepl("NA", unlist(r2))))
  expect_true(any(fit == "95% CI[.56,.83]"))
  expect_true(any(r2[, "Unique_95%_CI"] == "[.56, .83]"))

  # Smithson (2001): F(1, 42) = 6, 90% interval [.0117, .2801].
  eta <- get.ci.partial.eta.squared(F.value = 6, df1 = 1, df2 = 42, conf.level = .90)
  expect_equal(round(c(eta$LL, eta$UL), 4), c(.0117, .2801))

  d <- apa.d.table(iv = dose, dv = libido, data = viagra)$table_body
  expect_true(any(d == "[-0.55, 2.04]"))
  expect_false(any(grepl("NA", d)))
})
