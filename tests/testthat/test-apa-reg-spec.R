test_that("reg_col_spec maps b to italic-b, right aligned", {
  s <- reg_col_spec("b")
  expect_equal(s$align, "r")
  expect_equal(as_plaintext(s$label), "b")
  expect_true(s$label[[1]]$italic)
})

test_that("reg_col_spec b_CI is a 95% CI label, centered", {
  s <- reg_col_spec("b_CI")
  expect_equal(as_plaintext(s$label), "95% CI")
  expect_equal(s$align, "c")
})

test_that("reg_col_spec sr2_CI honors the prop.var conf level (90)", {
  expect_equal(as_plaintext(reg_col_spec("sr2_CI", 0.90)$label), "90% CI")
})

test_that("reg_col_spec sr2 header is 'Unique R2' with a superscript 2", {
  s <- reg_col_spec("sr2")
  expect_equal(as_plaintext(s$label), "Unique R2")
  scripts <- vapply(s$label, function(rn) rn$script, character(1))
  expect_true("super" %in% scripts)
})

test_that("reg_col_spec difference header is the delta-Fit glyph", {
  expect_equal(as_plaintext(reg_col_spec("difference")$label), "\u0394Fit")
})

test_that("reg_col_spec errors on an unknown key", {
  expect_error(reg_col_spec("nope"), "unknown column key")
})

test_that("apa.reg.table attaches a well-formed spec (single block)", {
  blk1 <- lm(sales ~ adverts + airplay, data = album)
  tbl <- apa.reg.table(blk1, table.number = 1)
  expect_s3_class(tbl, "apa_table")
  s <- tbl$spec
  expect_s3_class(s, "apa_table_spec")
  expect_silent(validate_apa_table_spec(s))
  expect_equal(as_plaintext(s$title), "Regression Predicting Sales")
  expect_equal(s$number, 1L)
  expect_true(is.na(s$landscape))
  expect_length(s$row_groups, 0)
  labs <- vapply(s$col_labels, as_plaintext, character(1))
  expect_equal(labs[1], "Predictor")
  expect_true("Unique R2" %in% labs)
  expect_false("\u0394Fit" %in% labs)   # single block has no difference column
  note <- as_plaintext(s$notes[[1]])
  expect_match(note, "^N = 200\\.")
  # significance stars are a separate note entry (their own line in Word)
  expect_length(s$notes, 2L)
  expect_match(as_plaintext(s$notes[[2]]), "^\\* indicates")
  expect_match(as_plaintext(s$notes[[2]]), "p < \\.01\\.$")
  expect_match(apa_note_text(s), "p < \\.01\\.$")   # apaquarto string still ends with stars
  expect_false(grepl("Note\\.", note))   # no "Note." prefix in the spec
})

test_that("apa.reg.table spec encodes model blocks as row groups (multi-block)", {
  blk1 <- lm(sales ~ adverts + airplay, data = album)
  blk2 <- lm(sales ~ adverts + airplay + attract, data = album)
  s <- apa.reg.table(blk1, blk2, table.number = 2)$spec
  expect_length(s$row_groups, 2)
  expect_equal(vapply(s$row_groups, function(g) as_plaintext(g$label), character(1)),
               c("Model 1", "Model 2"))
  labs <- vapply(s$col_labels, as_plaintext, character(1))
  expect_true("\u0394Fit" %in% labs)
  first_col <- vapply(s$body, function(row) as_plaintext(row[[1]]), character(1))
  expect_false(any(grepl("^Model [0-9]+$", first_col)))  # label rows removed from body
  # row-group indices cover the body without referencing label rows
  expect_true(all(unlist(lapply(s$row_groups, `[[`, "rows")) %in% seq_along(s$body)))
})

test_that("apa.reg.table number is 1 by default and NA when suppressed", {
  blk1 <- lm(sales ~ adverts + airplay, data = album)
  expect_equal(apa.reg.table(blk1)$spec$number, 1L)
  expect_true(is.na(apa.reg.table(blk1, table.number = 0)$spec$number))
})

test_that("attaching $spec leaves the legacy fields intact", {
  blk1 <- lm(sales ~ adverts + airplay, data = album)
  tbl <- apa.reg.table(blk1, table.number = 1)
  expect_true(all(c("table_title", "table_body", "table_note",
                    "latex.body") %in% names(tbl)))
})

test_that("apa.reg.table landscape option reaches the object and the spec", {
  blk1 <- lm(sales ~ adverts + airplay, data = album)
  t_default <- apa.reg.table(blk1)
  expect_true(is.na(t_default$landscape))
  expect_true(is.na(t_default$spec$landscape))
  t_port <- apa.reg.table(blk1, landscape = FALSE)
  expect_false(t_port$landscape)
  expect_false(t_port$spec$landscape)
})
