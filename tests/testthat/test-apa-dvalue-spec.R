test_that("apa.d.table attaches a well-formed spec (woven d/CI -> multi-line cells)", {
  tbl <- apa.d.table(iv = dose, dv = libido, data = viagra)
  s <- tbl$spec
  expect_s3_class(s, "apa_table_spec")
  expect_silent(validate_apa_table_spec(s))
  expect_length(s$col_labels, 5)
  expect_equal(vapply(s$col_labels, as_plaintext, character(1)),
               c("Variable", "M", "SD", "1", "2"))
  expect_equal(s$col_align, c("l", "r", "r", "r", "r"))
  expect_length(s$body, 3)
  # row 1 Placebo: descriptives present, both d cells blank
  expect_equal(as_plaintext(s$body[[1]][[1]]), "1. Placebo")
  expect_equal(as_plaintext(s$body[[1]][[2]]), "2.20")
  expect_equal(trimws(as_plaintext(s$body[[1]][[4]])), "")
  # row 2 Low Dose: col "1" stacks d over [CI]
  cell <- as_plaintext(s$body[[2]][[4]])
  expect_match(cell, "0.77", fixed = TRUE)
  expect_match(cell, "[-0.55, 2.04]", fixed = TRUE)
  expect_true(grepl("\n", cell))                 # two lines in one cell
  expect_equal(trimws(as_plaintext(s$body[[2]][[5]])), "")  # col "2" blank
  # row 3 High Dose: both d cells populated
  expect_match(as_plaintext(s$body[[3]][[4]]), "1.93", fixed = TRUE)
  expect_match(as_plaintext(s$body[[3]][[5]]), "1.24", fixed = TRUE)
  # title carries italic d; note has no "Note." prefix and cites Borenstein
  # Generated from the iv and dv rather than a stock string.
  expect_equal(as_plaintext(s$title),
               "Means, Standard Deviations, and d-Values for Libido by Dose")
  expect_false(grepl("Note\\.", as_plaintext(s$notes[[1]])))
  expect_match(as_plaintext(s$notes[[1]]), "Borenstein")
  expect_length(s$spanners, 0)
  expect_true(is.na(s$landscape))
})
