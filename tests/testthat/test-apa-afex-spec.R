test_that("apa.afex.table attaches a well-formed spec (between-subjects)", {
  skip_if_not_installed("afex")
  goggles_results <- afex::aov_ez(id = "participant", dv = "attractiveness",
                                  data = goggles, between = c("gender", "alcohol"))
  tbl <- apa.afex.table(afex.output = goggles_results)
  s <- tbl$spec
  expect_s3_class(s, "apa_table_spec")
  expect_silent(validate_apa_table_spec(s))
  expect_length(s$col_labels, 7)
  expect_equal(vapply(s$col_labels, as_plaintext, character(1)),
               c("Predictor", "df_num", "df_den", "MSE", "F", "p", "eta2g"))
  expect_equal(s$col_align[1], "l")
  expect_length(s$body, 3)
  preds <- vapply(s$body, function(r) as_plaintext(r[[1]]), character(1))
  expect_true(all(c("gender", "alcohol", "gender x alcohol") %in% preds))
  expect_length(s$spanners, 0)
  expect_true(is.na(s$landscape))
  expect_false(grepl("Note\\.", as_plaintext(s$notes[[1]])))
  expect_match(as_plaintext(s$notes[[1]]), "generalized eta-squared")
  expect_false(grepl("Greenhouse", as_plaintext(s$notes[[1]])))  # no Epsilon sentence
})

test_that("apa.afex.table spec: within-subjects + GG correction (8-col Epsilon layout)", {
  skip_if_not_installed("afex")
  skip_if_not_installed("tidyr")
  drink_attitude_wide <- apaTables::drink_attitude_wide
  drink_attitude_long <- tidyr::pivot_longer(drink_attitude_wide,
                                cols = beer_positive:water_neutral,
                               names_to = c("drink", "imagery"),
                               names_sep = "_",
                                values_to = "attitude")
  drink_attitude_long$drink   <- as.factor(drink_attitude_long$drink)
  drink_attitude_long$imagery <- as.factor(drink_attitude_long$imagery)
  drink_attitude_results <- afex::aov_ez(id = "participant", dv = "attitude",
                               data = drink_attitude_long,
                               within = c("drink", "imagery"))
  tbl <- apa.afex.table(afex.output = drink_attitude_results, correction = "GG")
  s <- tbl$spec

  # 8-column layout: Predictor df_num df_den Epsilon MSE F p eta2g
  expect_length(s$col_labels, 8)
  col_plains <- vapply(s$col_labels, as_plaintext, character(1))
  expect_equal(col_plains[4], "epsilon")   # 4th label is the Epsilon symbol
  expect_equal(col_plains, c("Predictor", "df_num", "df_den", "epsilon", "MSE", "F", "p", "eta2g"))

  # Body: 3 within-subject effects
  expect_length(s$body, 3)
  preds <- vapply(s$body, function(r) as_plaintext(r[[1]]), character(1))
  expect_true(all(c("drink", "imagery", "drink x imagery") %in% preds))

  # Note: names GG correctly; no "Note." prefix; ends with eta-squared sentence
  note_txt <- as_plaintext(s$notes[[1]])
  expect_match(note_txt, "Greenhouse-Geisser")
  expect_false(grepl("Huynh-Feldt", note_txt))
  expect_false(grepl("Note\\.", note_txt))
  expect_match(note_txt, "generalized eta-squared")

  # Spec is valid
  expect_silent(validate_apa_table_spec(s))
})
