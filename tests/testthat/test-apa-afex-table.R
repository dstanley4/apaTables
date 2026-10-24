library(apaTables)

# Helper to create between-subjects afex result (goggles)
make_goggles_afex <- function() {
     afex::aov_ez("participant", "attractiveness", goggles,
                  between = c("gender", "alcohol"))
}

# Helper to create within-subjects afex result (drink_attitude)
make_drink_afex <- function() {
     drink_attitude_long <- tidyr::pivot_longer(drink_attitude_wide,
                                                 cols = beer_positive:water_neutral,
                                                 names_to = c("drink", "imagery"),
                                                 names_sep = "_",
                                                 values_to = "attitude")
     drink_attitude_long$drink <- as.factor(drink_attitude_long$drink)
     drink_attitude_long$imagery <- as.factor(drink_attitude_long$imagery)

     afex::aov_ez("participant", "attitude", drink_attitude_long,
                  within = c("drink", "imagery"))
}

# Helper to create mixed design afex result (dating)
make_dating_afex <- function() {
     dating_long <- tidyr::pivot_longer(dating_wide,
                                         cols = attractive_high:ugly_none,
                                         names_to = c("looks", "personality"),
                                         names_sep = "_",
                                         values_to = "date_rating")
     dating_long$looks <- as.factor(dating_long$looks)
     dating_long$personality <- as.factor(dating_long$personality)

     afex::aov_ez("participant", "date_rating", dating_long,
                  between = "gender",
                  within = c("looks", "personality"))
}


test_that("apa.afex.table works with between-subjects design (goggles)", {
     skip_if_not_installed("afex")
     goggles_results <- suppressMessages(make_goggles_afex())
     t1 <- apa.afex.table(goggles_results)
     expect_s3_class(t1, "apa_table")
     expect_equal(t1$table.type, "afex")
})

test_that("apa.afex.table between-subjects has correct predictors", {
     skip_if_not_installed("afex")
     goggles_results <- suppressMessages(make_goggles_afex())
     t1 <- apa.afex.table(goggles_results)
     predictors <- t1$table_body$Predictor
     expect_true("gender" %in% predictors)
     expect_true("alcohol" %in% predictors)
     expect_true("gender x alcohol" %in% predictors)
})

test_that("apa.afex.table between-subjects has expected columns (has MSE, no Epsilon)", {
     skip_if_not_installed("afex")
     goggles_results <- suppressMessages(make_goggles_afex())
     t1 <- apa.afex.table(goggles_results)
     col_names <- names(t1$table_body)
     expect_true("Predictor" %in% col_names)
     expect_true("MSE" %in% col_names)
     expect_true("F" %in% col_names)
     expect_true("p" %in% col_names)
     expect_true("ges" %in% col_names)
     expect_false("Epsilon" %in% col_names)
})

test_that("apa.afex.table between-subjects has integer df", {
     skip_if_not_installed("afex")
     goggles_results <- suppressMessages(make_goggles_afex())
     t1 <- apa.afex.table(goggles_results)
     col_names <- names(t1$table_body)
     expect_true("df_num" %in% col_names)
     expect_true("df_den" %in% col_names)
     # Check that df values are integer-formatted (no decimal places like "1.00")
     df_num_values <- t1$table_body$df_num
     expect_false(any(grepl("\\.", df_num_values)))
})

test_that("apa.afex.table works with within-subjects design", {
     skip_if_not_installed("afex")
     skip_if_not_installed("tidyr")
     drink_results <- suppressMessages(make_drink_afex())
     t1 <- apa.afex.table(drink_results)
     expect_s3_class(t1, "apa_table")
     expect_equal(t1$table.type, "afex")
})

test_that("apa.afex.table within-subjects has correct effects", {
     skip_if_not_installed("afex")
     skip_if_not_installed("tidyr")
     drink_results <- suppressMessages(make_drink_afex())
     t1 <- apa.afex.table(drink_results)
     predictors <- t1$table_body$Predictor
     expect_true("drink" %in% predictors)
     expect_true("imagery" %in% predictors)
     expect_true("drink x imagery" %in% predictors)
})

test_that("apa.afex.table within-subjects has Epsilon column with GG correction", {
     skip_if_not_installed("afex")
     skip_if_not_installed("tidyr")
     drink_results <- suppressMessages(make_drink_afex())
     t1 <- apa.afex.table(drink_results, correction = "GG")
     col_names <- names(t1$table_body)
     expect_true("Epsilon" %in% col_names)
     expect_true("MSE" %in% col_names)
})

test_that("apa.afex.table works with mixed design", {
     skip_if_not_installed("afex")
     skip_if_not_installed("tidyr")
     dating_results <- suppressMessages(make_dating_afex())
     t1 <- apa.afex.table(dating_results)
     expect_s3_class(t1, "apa_table")
     expect_equal(t1$table.type, "afex")
})

test_that("epsilon is derived from the df, so it agrees with them and never exceeds 1", {
     # Epsilon is a sphericity correction and CANNOT exceed 1, but the
     # Huynh-Feldt ESTIMATOR can. Do not read it from
     # summary()$pval.adjustments: that returns it UNCAPPED (1.0744 for
     # `looks` here) while afex has already used 1 for the df -- so the table
     # would print an impossible epsilon next to df that do not incorporate
     # it, contradicting its own note. It is DERIVED as corrected df over
     # uncorrected df, which is the multiplier actually applied.
     skip_if_not_installed("afex")
     skip_if_not_installed("tidyr")
     fit <- suppressMessages(make_dating_afex())

     for (cc in c("GG", "HF")) {
          t1 <- suppressWarnings(apa.afex.table(fit, correction = cc))
          b  <- t1$table_body
          e  <- suppressWarnings(as.numeric(b$Epsilon))
          w  <- !is.na(e)
          expect_gt(sum(w), 0L)                      # must have something to check
          expect_true(all(e[w] <= 1),
                      info = paste(cc, "epsilon out of range:",
                                   paste(e[w], collapse = ", ")))

          # THE TABLE AGREES WITH ITSELF: the displayed df_num is the
          # uncorrected df times the displayed epsilon.
          #
          # The tolerance is DERIVED, not guessed. Both columns are rounded to
          # 2 dp, so df_shown = k*eps_true +/- 0.005 and eps_shown = eps_true
          # +/- 0.005, giving a worst case of 0.005 * (1 + k) -- 0.025 at k = 4.
          # A tolerance of 0.01 fails on `personality`, where 1.87 is shown
          # against 2 x 0.93 = 1.86, purely from rounding.
          dfn <- suppressWarnings(as.numeric(b$df_num[w]))
          k   <- round(dfn / e[w])
          expect_true(all(abs(dfn - k * e[w]) <= 0.005 * (1 + k) + 1e-9),
                      info = paste(cc, "df and epsilon disagree:",
                                   paste(sprintf("%.2f vs %d x %.2f", dfn, k, e[w]),
                                         collapse = "; ")))
     }
     # The row that used to print 1.07 now reads exactly 1.
     hf <- suppressWarnings(apa.afex.table(fit, correction = "HF"))
     expect_true(any(abs(suppressWarnings(as.numeric(hf$table_body$Epsilon)) - 1) < 1e-8,
                     na.rm = TRUE))
})

test_that("the derived epsilon matches afex's own, computed the old way", {
     # AN INDEPENDENT ORACLE, not a self-consistency check.
     #
     # The test above proves the table agrees with ITSELF -- the epsilon shown
     # matches the df shown. It would still pass if the df-ratio derivation
     # were systematically wrong, because both columns come from the same
     # place. This one compares against afex's OWN epsilon, which it computes
     # from the sphericity estimate rather than from a df ratio -- so the two
     # agree for reasons independent of our reasoning.
     #
     # `summary()$pval.adjustments` is the ORIGINAL source apaTables read, and
     # reading it is exactly what raised "HF eps > 1 treated as 1" on every
     # call. Suppressed here on purpose: the warning is the thing this test
     # exists to have removed from the package, and re-raising it in the suite
     # would undo that.
     #
     # THE ONE DELIBERATE DIFFERENCE is HF above 1. afex returns the estimator
     # uncapped; we show the multiplier actually applied, which is capped at 1.
     # Encoding it here states that we differ THERE and nowhere else.
     skip_if_not_installed("afex")
     skip_if_not_installed("tidyr")
     fit <- suppressMessages(make_dating_afex())
     adj <- suppressWarnings(summary(fit)$pval.adjustments)

     # If afex ever renames these, this test SHOULD fail -- the derivation was
     # validated against them and would need re-validating.
     expect_true(all(c("GG eps", "HF eps") %in% colnames(adj)),
                 info = "afex changed pval.adjustments; re-validate the derivation")

     for (cc in c("GG", "HF")) {
          t1  <- suppressWarnings(apa.afex.table(fit, correction = cc))
          b   <- t1$table_body
          col <- if (cc == "GG") "GG eps" else "HF eps"

          checked <- 0L
          for (i in seq_len(nrow(b))) {
               nm <- b$Predictor[i]
               # the table prints "gender x looks"; afex uses "gender:looks"
               key <- gsub(" x ", ":", nm, fixed = TRUE)
               if (!(key %in% rownames(adj))) next
               shown <- trimws(b$Epsilon[i])
               if (!nzchar(shown)) next
               expected <- min(adj[key, col], 1)   # the cap, and only here
               # Compared at DISPLAY precision, so no tolerance is needed and
               # none can be got subtly wrong.
               expect_equal(shown, sprintf("%.2f", expected),
                            info = paste(cc, nm, "-- afex says",
                                         signif(adj[key, col], 6)))
               checked <- checked + 1L
          }
          # Both branches must actually have been exercised: this design has
          # HF eps 1.0744 on `looks` (capped) and 0.9922 on `looks:personality`
          # (not capped), so a run that checked nothing is a broken test.
          expect_gt(checked, 1L)
     }
})

test_that("a between-subjects design has no epsilon column at all", {
     # The third branch. No within factors means no sphericity correction to
     # report, so there is nothing to derive and nothing to display.
     skip_if_not_installed("afex")
     fit <- suppressMessages(make_goggles_afex())
     t1 <- suppressWarnings(apa.afex.table(fit))
     expect_false("Epsilon" %in% names(t1$table_body))
})

test_that("the GG default raises no warning, and the HF path still reports its cap", {
     # The warning "HF eps > 1 treated as 1" used to fire on the DEFAULT path,
     # about a number the default table never shows, because summary() computes
     # the whole adjustments table. Deriving epsilon from anova() removed the
     # cause: anova(correction = "none") and anova(correction = "GG") are both
     # silent. On the HF path afex still says it capped, and it SHOULD -- the
     # user asked for HF.
     skip_if_not_installed("afex")
     skip_if_not_installed("tidyr")
     fit <- suppressMessages(make_dating_afex())

     expect_no_warning(apa.afex.table(fit))                        # GG default
     expect_no_warning(apa.afex.table(fit, correction = "GG"))
     expect_no_warning(apa.afex.table(fit, correction = "none"))
     expect_warning(apa.afex.table(fit, correction = "HF"), "HF eps")
})

test_that("apa.afex.table mixed design has both between and within predictors", {
     skip_if_not_installed("afex")
     skip_if_not_installed("tidyr")
     dating_results <- suppressMessages(make_dating_afex())
     t1 <- apa.afex.table(dating_results)
     predictors <- t1$table_body$Predictor
     expect_true("gender" %in% predictors)
     expect_true("looks" %in% predictors)
     expect_true("personality" %in% predictors)
})

test_that("apa.afex.table HF correction works", {
     skip_if_not_installed("afex")
     skip_if_not_installed("tidyr")
     drink_results <- suppressMessages(make_drink_afex())
     t1 <- apa.afex.table(drink_results, correction = "HF")
     col_names <- names(t1$table_body)
     expect_true("Epsilon" %in% col_names)
     expect_s3_class(t1, "apa_table")
})

test_that("apa.afex.table correction = 'none' omits Epsilon", {
     skip_if_not_installed("afex")
     skip_if_not_installed("tidyr")
     drink_results <- suppressMessages(make_drink_afex())
     t1 <- apa.afex.table(drink_results, correction = "none")
     col_names <- names(t1$table_body)
     expect_false("Epsilon" %in% col_names)
})

test_that("apa.afex.table replaces .000 p-values with <.001", {
     skip_if_not_installed("afex")
     skip_if_not_installed("tidyr")
     drink_results <- suppressMessages(make_drink_afex())
     t1 <- apa.afex.table(drink_results)
     p_values <- t1$table_body$p
     expect_false(any(p_values == ".000"))
})

test_that("apa.afex.table has LaTeX components", {
     skip_if_not_installed("afex")
     goggles_results <- suppressMessages(make_goggles_afex())
     t1 <- apa.afex.table(goggles_results)
     expect_false(is.null(t1$latex.body))
     expect_false(is.null(t1$latex.table.note))
     expect_true(is.na(t1$landscape))
})

test_that("apa.afex.table landscape option reaches the object and the spec", {
     skip_if_not_installed("afex")
     goggles_results <- suppressMessages(make_goggles_afex())
     t_default <- apa.afex.table(goggles_results)
     expect_true(is.na(t_default$landscape))
     expect_true(is.na(t_default$spec$landscape))
     t_land <- apa.afex.table(goggles_results, landscape = TRUE)
     expect_true(t_land$landscape)
     expect_true(t_land$spec$landscape)
})

test_that("apa.afex.table table.title reaches the Word/HTML/Quarto title", {
     skip_if_not_installed("afex")
     goggles_results <- suppressMessages(make_goggles_afex())
     t_default <- apa.afex.table(goggles_results)
     expect_equal(t_default$latex.table.title, "ANOVA Results")
     expect_equal(apa.caption(t_default), "ANOVA Results")
     t_custom <- apa.afex.table(goggles_results,
                                table.title = "Effect of Alcohol on Attractiveness Ratings")
     expect_equal(t_custom$table_title,
                  "Effect of Alcohol on Attractiveness Ratings")
     expect_equal(t_custom$latex.table.title,
                  "Effect of Alcohol on Attractiveness Ratings")
     expect_equal(apa.caption(t_custom),
                  "Effect of Alcohol on Attractiveness Ratings")
})

test_that("apa.afex.table table.number and table.title parameters work", {
     skip_if_not_installed("afex")
     goggles_results <- suppressMessages(make_goggles_afex())
     t1 <- apa.afex.table(goggles_results, table.number = 5,
                           table.title = "Custom AFEX Title")
     expect_equal(t1$table_number, 5)
     expect_equal(t1$table_title, "Custom AFEX Title")
})

test_that("apa.afex.table file output writes and cleans up", {
     skip_if_not_installed("afex")
     goggles_results <- suppressMessages(make_goggles_afex())
     tmp_file <- file.path(tempdir(), "test_afex_table.docx")
     t1 <- apa.afex.table(goggles_results, filename = tmp_file)
     expect_true(file.exists(tmp_file))
     unlink(tmp_file)
})

test_that("apa.afex.table filename defaults to NA like every other builder (docs audit)", {
     skip_if_not_installed("afex")
     # Same normalization as apa.aov.table's (see test-apa-aov-table.R):
     # bare `filename` + missing() guard became `filename = NA` + is.na()
     # guard, so the printed signature stops claiming the argument is
     # required and an explicit NA means "no file".
     afex_result <- make_goggles_afex()
     expect_identical(formals(apa.afex.table)$filename, NA)
     withr::with_tempdir({
          t_default <- apa.afex.table(afex_result)
          t_na <- apa.afex.table(afex_result, filename = NA)
          expect_equal(length(list.files()), 0)
          expect_equal(t_na$table_body, t_default$table_body)
          apa.afex.table(afex_result, filename = "afex.docx")
          expect_true(file.exists("afex.docx"))
     })
})
