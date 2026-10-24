# What the tables report, and what their notes say about it: group sizes,
# absolute pooled-SD d-values, pairwise n, interval bounds, star
# thresholds, and notes that define only the columns shown.

unequal_var_data <- function() {
  set.seed(1)
  data.frame(g = rep(c("a", "b", "c"), each = 40),
             y = c(stats::rnorm(40, 0, 1), stats::rnorm(40, 1, 3.5), stats::rnorm(40, .5, 1)))
}

test_that("apa.d.table() reports a pooled-SD d-value for every pair, with no variance mark", {
  t1 <- apa.d.table(iv = g, dv = y, data = unequal_var_data())
  # b has a much larger spread than a and c; every pair still gets the
  # pooled-SD d, unmarked
  body <- t1$table_body
  # rows: a, a blank CI line, b, the CI line for b, a spacer, c, the CI line for c
  expect_equal(unname(body[3, "1"]), "0.56")    # b vs a
  expect_equal(unname(body[6, "2"]), "0.34")    # c vs b
  expect_equal(unname(body[6, "1"]), "0.60")    # c vs a
  expect_false(any(grepl("a$", trimws(body[, c("1", "2")]))))
  d <- unequal_var_data()
  expect_equal(unname(body[3, "1"]),
               sprintf("%1.2f", get_d_value(d$y[d$g == "b"], d$y[d$g == "a"])))
  # in the spec each d is plain text, with no superscript run
  s <- t1$spec
  d_line <- unclass(s$body[[2]][[5]])[[1]]
  expect_true(is.character(unclass(d_line)))
  expect_equal(as_plaintext(d_line), "0.56")
  expect_length(s$notes, 1)
  expect_match(as_plaintext(s$notes[[1]]), "d-values are estimates calculated with the pooled standard deviation", fixed = TRUE)
  expect_false(grepl("variances", t1$table_note))
  expect_false(grepl("Levene", t1$table_note))
})

test_that("apa.d.table() says its d-values are absolute, and has no flag note without a flag", {
  t1 <- apa.d.table(iv = dose, dv = libido, data = viagra)
  expect_length(t1$spec$notes, 1)
  expect_match(as_plaintext(t1$spec$notes[[1]]), "d-values are absolute values; the direction of each difference can be read from the means.", fixed = TRUE)
  expect_false(grepl("variances", t1$table_note))
})

test_that("apa.d.table() and apa.1way.table() count each group's cases with a value", {
  v <- viagra
  v$libido[c(1, 2)] <- NA
  expect_equal(unname(apa.d.table(iv = dose, dv = libido, data = v)$table_body[1, "n"]), "3")
  expect_equal(apa.1way.table(iv = dose, dv = libido, data = v)$table_body$n, c("3", "5", "5"))
})

test_that("the two-way note describes marginal means without claiming main effects", {
  t2 <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                       data = goggles, show.marginal.means = TRUE)
  note <- as_plaintext(t2$spec$notes[[1]])
  expect_false(grepl("main effect", note))
  expect_match(note, "for each level of a factor, across all levels of the other factor", fixed = TRUE)
  expect_false(grepl("main effect", t2$table_note))
  t2ci <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                         data = goggles, show.conf.interval = TRUE)
  expect_match(as_plaintext(t2ci$spec$notes[[1]]),
               "CI = 95% confidence interval for each mean.", fixed = TRUE)
})

test_that("no table note gives a statistic ownership with the word own", {
  m1 <- lm(rating ~ complaints, data = attitude)
  m2 <- lm(rating ~ complaints + privileges, data = attitude)
  a <- attitude[, 1:3]
  a$rating[2] <- NA
  v <- c(1, 2, 3, 4, 5, 10, 20, 30, 40, 50, 3, 3.1, 3.2, 3.3, 3.4)
  g <- factor(rep(c("a", "b", "c"), each = 5))
  tables <- list(
    apa.cor.table(data = a),
    apa.d.table(iv = g, dv = v, data = data.frame(g = g, v = v)),
    apa.1way.table(iv = dose, dv = libido, data = viagra, show.conf.interval = TRUE),
    apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                   data = goggles, show.conf.interval = TRUE),
    apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                   data = goggles, show.marginal.means = TRUE),
    apa.reg.table(m1, m2),
    apa.aov.table(lm(attractiveness ~ gender * alcohol, data = goggles)))
  for (tbl in tables) {
    notes <- c(vapply(tbl$spec$notes, as_plaintext, character(1)),
               tbl$table_note, tbl$table.note)
    expect_false(any(grepl("\\bown\\b", notes, ignore.case = TRUE)), info = notes[1])
  }
})

test_that("with missing values the correlation note gives the range of pairwise n", {
  a <- attitude[, 1:3]
  a$rating[1:5] <- NA
  a$complaints[6:10] <- NA
  t1 <- apa.cor.table(data = a)
  expect_match(t1$table_note, "n ranged from 20 to 25.", fixed = TRUE)
  expect_match(as_plaintext(t1$spec$notes[[1]]), "Missing data were handled with pairwise deletion; n ranged from 20 to 25.", fixed = TRUE)
  # complete data: no such sentence
  expect_false(grepl("pairwise", apa.cor.table(data = attitude[, 1:3])$table_note))
  # every pair the same size
  expect_equal(cor_pair_n_text(c(20, 20, 20), TRUE),
               "Missing data were handled with pairwise deletion; n = 20 for every correlation.")
  expect_equal(cor_pair_n_text(c(20, 25), FALSE), "")
})

test_that("p = .01 exactly takes one star, below it two", {
  expect_equal(add.sig.stars(c(".50", ".50", ".50", ".50"), c(.0500001, .05, .01, .0099999)),
               c(".50", ".50", ".50*", ".50**"))
  expect_equal(add.sig.stars(".50", .049), ".50*")
  expect_equal(add.sig.stars.latex(c(".50", ".50"), c(.01, .0099999)),
               c(".50$^{*}$", ".50$^{**}$"))
})

test_that("the Alf-Graf interval for a proportion of variance stays within 0 and 1", {
  ci <- get_deltaR2_ci(R2_2 = .026, R2_1 = .024, n = 30)
  expect_identical(ci$LL, 0)
  expect_gt(ci$UL, 0)
  set.seed(3)
  d <- data.frame(a = stats::rnorm(30), b = stats::rnorm(30))
  d$y <- .3 * d$a + stats::rnorm(30)
  t1 <- apa.reg.table(lm(y ~ a, d), lm(y ~ a + b, d))
  shown <- unlist(t1$table_body[, c("Unique_95%_CI", "Delta_Fit")])
  expect_false(any(grepl("[-.", shown, fixed = TRUE)))
  expect_false(any(grepl("[-0", shown, fixed = TRUE)))
})

test_that("the regression note defines beta and r only when the table shows them", {
  check <- function(t1, beta, r) {
    for (note in list(t1$table_note, as_plaintext(t1$spec$notes[[1]]))) {
      expect_identical(grepl("(beta|\u03b2) = standardized regression weight", note), beta)
      expect_identical(grepl("zero-order correlation", note), r)
    }
    expect_identical("beta" %in% names(t1$table_body), beta)
    expect_identical("r" %in% names(t1$table_body), r)
  }
  check(apa.reg.table(lm(mpg ~ wt + qsec, mtcars)), TRUE, TRUE)
  check(apa.reg.table(lm(mpg ~ wt + factor(cyl), mtcars)), FALSE, FALSE)
  check(apa.reg.table(lm(mpg ~ wt + qsec, mtcars, weights = cyl)), FALSE, TRUE)
  check(apa.reg.table(lm(mpg ~ wt, mtcars), lm(mpg ~ wt + factor(cyl), mtcars)), FALSE, FALSE)
})

test_that("two-way marginal means use only the cases in the cells", {
  d <- data.frame(a = rep(c("a1", "a2"), each = 6),
                  b = rep(rep(c("b1", "b2"), each = 3), 2),
                  y = c(1:6, 11:16))
  stray <- rbind(d, data.frame(a = "a1", b = NA, y = 100),
                 data.frame(a = NA, b = "b1", y = 100))
  marg <- function(data) apa.2way.table(iv1 = a, iv2 = b, dv = y, data = data,
                                        show.marginal.means = TRUE)$table_body
  expect_identical(marg(stray), marg(d))
  expect_equal(unname(unlist(marg(d)[3, 6:7])), c("3.50", "1.87"))
})
