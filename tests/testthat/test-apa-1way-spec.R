test_that("apa.1way.table attaches a well-formed spec (no CI)", {
  tbl <- apa.1way.table(iv = dose, dv = libido, data = viagra, table.number = 1)
  expect_s3_class(tbl, "apa_table")
  s <- tbl$spec
  expect_s3_class(s, "apa_table_spec")
  expect_silent(validate_apa_table_spec(s))
  expect_equal(s$number, 1L)
  expect_length(s$col_labels, 3)
  expect_equal(vapply(s$col_labels, as_plaintext, character(1)), c("dose", "M", "SD"))
  expect_equal(s$col_align, c("l", "r", "r"))
  expect_length(s$body, 3)
  expect_equal(as_plaintext(s$body[[1]][[1]]), "Placebo")
  expect_equal(as_plaintext(s$body[[1]][[2]]), "2.20")
  expect_equal(as_plaintext(s$body[[3]][[1]]), "High Dose")
  expect_length(s$spanners, 0)
  expect_length(s$row_groups, 0)
  expect_false(grepl("Note\\.", as_plaintext(s$notes[[1]])))
  expect_match(as_plaintext(s$notes[[1]]), "mean")
})

test_that("apa.1way.table spec handles the CI column (order M, CI, SD)", {
  tbl <- apa.1way.table(iv = dose, dv = libido, data = viagra,
                        show.conf.interval = TRUE)
  s <- tbl$spec
  expect_silent(validate_apa_table_spec(s))
  expect_length(s$col_labels, 4)
  expect_equal(vapply(s$col_labels, as_plaintext, character(1)),
               c("dose", "M", "95% CI", "SD"))
  expect_match(as_plaintext(s$body[[1]][[3]]), "[", fixed = TRUE)  # CI bracket
  expect_match(as_plaintext(s$notes[[1]]), "confidence interval")
})

test_that("attaching $spec leaves the legacy one-way fields intact", {
  tbl <- apa.1way.table(iv = dose, dv = libido, data = viagra)
  expect_true(all(c("table_body", "table_note", "latex.column.labels",
                    "latex.column.centering", "table.type") %in% names(tbl)))
})
