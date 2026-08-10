test_that("console renderer prints number, title, body, note", {
  s <- apa_spec_start(2) |>
    apa_col("Predictor", "l", list("Adverts", "Airplay")) |>
    apa_col(apa_sym("b"), "r", list(richtext(".08"), richtext(".90"))) |>
    apa_note("* p < .05.") |>
    apa_spec_finish(title = "Regression of Sales", number = 2L)

  out <- paste(capture.output(print(s)), collapse = "\n")
  expect_match(out, "Table 2")
  expect_match(out, "Regression of Sales")
  expect_match(out, "Predictor")
  expect_match(out, "Adverts")
  expect_match(out, "Note\\. \\* p < .05\\.")
})

test_that("console renderer handles blank cells (correlation lower triangle)", {
  # a spec with empty cells used to crash width_of: max(integer(0)) -> -Inf (double),
  # which violated vapply(col, width_of, integer(1)).
  s <- apa_spec_start(2) |>
    apa_col("Variable", "l", list("1. x", "2. y")) |>
    apa_col("1", "r", list(richtext(""), richtext(".50"))) |>
    apa_col("2", "r", list(richtext(""), richtext(""))) |>
    apa_spec_finish(title = "Correlations")
  expect_no_error(print(s))

  # and the real builder path that exposed it
  expect_no_error(print(apa.cor.table(attitude)$spec))
})

test_that("console renderer is stable (snapshot)", {
  s <- apa_spec_start(2) |>
    apa_col("Predictor", "l", list("Adverts", "Airplay")) |>
    apa_col(apa_sym("b"), "r", list(richtext(".08"), richtext(".90"))) |>
    apa_note("* p < .05.") |>
    apa_spec_finish(title = "Regression of Sales", number = 2L)
  expect_snapshot(print(s))
})
