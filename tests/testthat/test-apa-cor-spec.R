test_that("apa_sym('N') is an italic N", {
  s <- apa_sym("N")
  expect_equal(as_plaintext(s), "N")
  expect_true(s[[1]]$italic)
})

test_that("apa.cor.table attaches a well-formed correlation spec (attitude)", {
  tbl <- apa.cor.table(attitude, table.number = 1)
  expect_s3_class(tbl, "apa_table")
  s <- tbl$spec
  expect_s3_class(s, "apa_table_spec")
  expect_silent(validate_apa_table_spec(s))
  expect_equal(as_plaintext(s$title), "Descriptive Statistics and Correlations")
  expect_equal(s$number, 1L)
  # 7 variables -> Variable, N, M, SD, 1..6 = 10 columns
  labs <- vapply(s$col_labels, as_plaintext, character(1))
  expect_equal(labs[1:4], c("Variable", "N", "M", "SD"))
  expect_equal(labs[5:10], as.character(1:6))
  # one spec row per variable (no separate physical CI/p rows)
  expect_length(s$body, 7)
  # descriptive cells are single-line; first variable's M is the v3 value
  expect_equal(as_plaintext(s$body[[1]][[3]]), "64.63")
  # the complaints-rating cell stacks r over CI over p (defaults show all three)
  cell <- s$body[[2]][[5]]                 # variable 2, correlation column "1"
  expect_s3_class(cell, "apa_richtext_lines")
  cell_txt <- as_plaintext(cell)
  expect_match(cell_txt, "^\\.83\\*\\*")   # r with stars on line 1
  expect_match(cell_txt, "\\[\\.66, \\.91\\]")  # CI on a later line
})

test_that("correlation note has no 'Note.' prefix and mentions M, SD, CI", {
  s <- apa.cor.table(attitude)$spec
  note <- as_plaintext(s$notes[[1]])
  expect_false(grepl("Note\\.", note))
  expect_match(note, "number of cases")
  expect_match(note, "confidence interval")
  # significance stars are a separate note entry (their own line in Word)
  expect_length(s$notes, 2L)
  expect_match(as_plaintext(s$notes[[2]]), "^\\* indicates")
  expect_match(as_plaintext(s$notes[[2]]), "p < \\.01\\.$")
  expect_match(apa.note.text(s), "p < \\.01\\.$")   # apaquarto string still ends with stars
})

test_that("correlation spec number is 1 by default and NA when suppressed", {
  expect_equal(apa.cor.table(attitude)$spec$number, 1L)
  expect_true(is.na(apa.cor.table(attitude, table.number = 0)$spec$number))
})

test_that("show.conf.interval=FALSE is deprecated and ignored (CIs forced on)", {
  # passing FALSE now emits a deprecation message and is otherwise ignored
  expect_message(apa.cor.table(attitude, show.conf.interval = FALSE), "deprecated")

  s_false <- suppressMessages(apa.cor.table(attitude, show.conf.interval = FALSE))$spec
  s_true  <- apa.cor.table(attitude)$spec

  # the CI is still present: the note documents it and the cell stays multi-line
  expect_true(grepl("confidence interval", as_plaintext(s_false$notes[[1]])))
  expect_true(inherits(s_false$body[[2]][[5]], "apa_richtext_lines"))

  # FALSE produces the same spec cell as the default (TRUE)
  expect_identical(as_plaintext(s_false$body[[2]][[5]]),
                   as_plaintext(s_true$body[[2]][[5]]))
})

test_that("attaching $spec leaves the legacy correlation fields intact", {
  tbl <- apa.cor.table(attitude, table.number = 1)
  expect_true(all(c("table_title", "table_body", "table_note",
                    "latex.body", "table.type") %in% names(tbl)))
  expect_true(is.matrix(tbl$table_body))
})
