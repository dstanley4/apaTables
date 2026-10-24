test_that("apa.aov.table attaches a well-formed spec (1-way lm, .90 CI)", {
  op <- options(contrasts = c("contr.helmert", "contr.poly"))
  on.exit(options(op))
  lm_output <- lm(libido ~ dose, data = viagra)
  tbl <- apa.aov.table(lm_output, table.number = 4)
  s <- tbl$spec
  expect_s3_class(s, "apa_table_spec")
  expect_silent(validate_apa_table_spec(s))
  expect_equal(s$number, 4L)
  expect_length(s$col_labels, 8)
  expect_equal(vapply(s$col_labels, as_plaintext, character(1)),
               c("Predictor", "SS", "df", "MS", "F", "p", "eta2p", "90% CI"))
  expect_equal(s$col_align[1], "l")
  # (Intercept) suppressed; Error retained
  preds <- vapply(s$body, function(r) as_plaintext(r[[1]]), character(1))
  expect_false("(Intercept)" %in% preds)
  expect_true("Error" %in% preds)
  expect_true("dose" %in% preds)
  # dose row carries the partial-eta CI bracket
  dose_row <- s$body[[which(preds == "dose")]]
  expect_match(as_plaintext(dose_row[[8]]), "[", fixed = TRUE)
  # Error row has blank F / p / eta2 / CI
  err_row <- s$body[[which(preds == "Error")]]
  expect_equal(trimws(as_plaintext(err_row[[8]])), "")
  expect_false(grepl("Note\\.", as_plaintext(s$notes[[1]])))
  expect_match(as_plaintext(s$notes[[1]]), "Sum of squares")
})

test_that("apa.aov.table spec CI label tracks conf.level", {
  op <- options(contrasts = c("contr.helmert", "contr.poly"))
  on.exit(options(op))
  lm_output <- lm(libido ~ dose, data = viagra)
  tbl <- apa.aov.table(lm_output, conf.level = .95)
  s <- tbl$spec
  labs <- vapply(s$col_labels, as_plaintext, character(1))
  expect_equal(labs[8], "95% CI")
})

test_that("out-of-range conf.level is refused, not replaced", {
  lm_output <- lm(libido ~ dose, data = viagra)
  expect_error(apa.aov.table(lm_output, conf.level = .85), "conf.level must be .90 or .95")
})
