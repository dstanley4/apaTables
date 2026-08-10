make_demo_spec <- function() {
  apa_table_spec(
    body = list(
      list("Adverts", richtext(".08"), apa_ci(0.05, 0.11)),
      list("Airplay", richtext(".90"), apa_ci(0.70, 1.10))
    ),
    col_labels = list("Predictor", apa_sym("b"), apa_ci_label(.95)),
    col_align  = c("l", "r", "c"),
    title      = richtext("Regression of Sales"),
    notes      = list(richtext("* p < .05.")),
    number     = 2L
  )
}

test_that("apa_table_spec builds and validates", {
  s <- make_demo_spec()
  expect_s3_class(s, "apa_table_spec")
  expect_silent(validate_apa_table_spec(s))
  expect_equal(length(s$col_align), 3L)
})

test_that("validator rejects width mismatch", {
  s <- make_demo_spec()
  s$col_align <- c("l", "r")
  expect_error(validate_apa_table_spec(s), "align")
})

test_that("validator rejects bad alignment codes", {
  s <- make_demo_spec()
  s$col_align <- c("l", "r", "x")
  expect_error(validate_apa_table_spec(s), "l.*r.*c|alignment")
})

test_that("validator rejects a body row with the wrong width", {
  s <- make_demo_spec()
  s$body[[1]] <- list("Adverts", richtext(".08"))  # 2 cells, columns = 3
  expect_error(validate_apa_table_spec(s), "cell per column|row")
})

test_that("assembly helpers build an equivalent spec", {
  s <- apa_spec_start(n_rows = 2) |>
    apa_col("Predictor", align = "l", values = list("Adverts", "Airplay")) |>
    apa_col(apa_sym("b"), align = "r",
            values = list(richtext(".08"), richtext(".90"))) |>
    apa_col(apa_ci_label(.95), align = "c",
            values = list(apa_ci(0.05, 0.11), apa_ci(0.70, 1.10))) |>
    apa_row_group("Step 1", rows = 1:2) |>
    apa_note("* p < .05.") |>
    apa_spec_finish(title = "Regression of Sales", number = 2L)

  expect_s3_class(s, "apa_table_spec")
  expect_equal(length(s$col_labels), 3L)
  expect_equal(s$col_align, c("l", "r", "c"))
  expect_equal(length(s$body), 2L)
  expect_equal(length(s$body[[1]]), 3L)
  expect_s3_class(s$row_groups[[1]]$label, "apa_richtext")
  expect_equal(as_plaintext(s$notes[[1]]), "* p < .05.")
})

test_that("apa_spanner and apa_row_group coerce string labels to richtext", {
  s <- apa_spec_start(n_rows = 1) |>
    apa_col("Var", align = "l", values = list("A")) |>
    apa_col(apa_sym("M"), align = "r", values = list(richtext("1.0"))) |>
    apa_spanner("Descriptives", cols = 2) |>
    apa_row_group("Block 1", rows = 1) |>
    apa_spec_finish(number = 1L)
  expect_s3_class(s$spanners[[1]]$label, "apa_richtext")
  expect_s3_class(s$row_groups[[1]]$label, "apa_richtext")
  expect_equal(as_plaintext(s$spanners[[1]]$label), "Descriptives")
  expect_s3_class(s$body[[1]][[1]], "apa_richtext")  # values also coerced
})

test_that("as_spec returns a bare spec unchanged", {
  s <- apa_spec_start(1) |> apa_col("A", "l", list("x")) |> apa_spec_finish(number = 1L)
  expect_identical(as_spec(s), s)
})

test_that("as_spec unwraps a legacy apa_table carrying $spec", {
  s <- apa_spec_start(1) |> apa_col("A", "l", list("x")) |> apa_spec_finish(number = 1L)
  legacy <- structure(list(table_title = "t", spec = s), class = "apa_table")
  expect_identical(as_spec(legacy), s)
})

test_that("as_spec errors on an object that is neither", {
  expect_error(as_spec(structure(list(), class = "apa_table")), "spec")
  expect_error(as_spec(42), "spec")
})

test_that("apa_spanner records a level (default 1)", {
  s <- apa_spec_start(1) |> apa_col("A", "l", list("x")) |>
    apa_spanner("Top", cols = 1, level = 2) |>
    apa_spanner("Bot", cols = 1) |>
    apa_spec_finish(number = 1L)
  expect_equal(s$spanners[[1]]$level, 2)
  expect_equal(s$spanners[[2]]$level, 1)
})

test_that("validate_apa_table_spec rejects out-of-range spanner cols", {
  good <- apa_spec_start(2) |>
    apa_col("A", "l", c("1", "2")) |>
    apa_col("B", "r", c("3", "4")) |>
    apa_spanner("Span", cols = 1:2) |>
    apa_spec_finish(title = "T")
  expect_s3_class(good, "apa_table_spec")

  expect_error(
    apa_spec_start(2) |>
      apa_col("A", "l", c("1", "2")) |>
      apa_col("B", "r", c("3", "4")) |>
      apa_spanner("Span", cols = 2:3) |>
      apa_spec_finish(title = "T"),
    "spanner"
  )
})

test_that("validate_apa_table_spec rejects out-of-range row-group rows", {
  expect_error(
    apa_spec_start(2) |>
      apa_col("A", "l", c("1", "2")) |>
      apa_row_group("G", rows = 2:3) |>
      apa_spec_finish(title = "T"),
    "row group"
  )
})
