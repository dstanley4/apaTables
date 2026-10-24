library(apaTables)

test_that("apa.aov.table 1-way returns correct class and type", {
     options(contrasts = c("contr.helmert", "contr.poly"))
     lm1 <- lm(libido ~ dose, data = viagra)
     t1 <- apa.aov.table(lm1, table.number = 4)
     expect_s3_class(t1, "apa_table")
     # "aov", not the former "aovstats": the object field and the spec's
     # meta$table.type must be the SAME STRING, because the type-to-builder
     # map keys on the spec one and a reader has no way to know which is
     # which. Asserted as an identity below rather than as a literal here,
     # so a future divergence fails for the right reason.
     expect_equal(t1$table.type, "aov")
     expect_identical(t1$table.type, t1$spec$meta$table.type)
})

test_that("apa.aov.table 1-way has correct structure", {
     options(contrasts = c("contr.helmert", "contr.poly"))
     lm1 <- lm(libido ~ dose, data = viagra)
     t1 <- apa.aov.table(lm1, table.number = 4)
     expect_equal(t1$table_number, 4)
     expect_true(is.data.frame(t1$table_body))
     col_names <- names(t1$table_body)
     expect_true("Predictor" %in% col_names)
     expect_true("SS" %in% col_names)
     expect_true("df" %in% col_names)
     expect_true("MS" %in% col_names)
     expect_true("F" %in% col_names)
     expect_true("p" %in% col_names)
     expect_true("partial_eta2" %in% col_names)
})

test_that("apa.aov.table 1-way has dose and Error rows", {
     options(contrasts = c("contr.helmert", "contr.poly"))
     lm1 <- lm(libido ~ dose, data = viagra)
     t1 <- apa.aov.table(lm1)
     predictors <- t1$table_body$Predictor
     expect_true("dose" %in% predictors)
     expect_true("Error" %in% predictors)
     expect_equal(nrow(t1$table_body), 3)
})

test_that("apa.aov.table Error row has empty F and p", {
     options(contrasts = c("contr.helmert", "contr.poly"))
     lm1 <- lm(libido ~ dose, data = viagra)
     t1 <- apa.aov.table(lm1)
     error_row <- t1$table_body[t1$table_body$Predictor == "Error", ]
     expect_equal(error_row$F, "")
     expect_equal(error_row$p, "")
})

test_that("apa.aov.table 1-way returns correct F and p values", {
     options(contrasts = c("contr.helmert", "contr.poly"))
     lm1 <- lm(libido ~ dose, data = viagra)
     t1 <- apa.aov.table(lm1)
     dose_row <- t1$table_body[t1$table_body$Predictor == "dose", ]
     expect_equal(dose_row$F, "5.12")
     expect_equal(dose_row$p, ".025")
})

test_that("apa.aov.table 2-way has main effects and interaction", {
     options(contrasts = c("contr.helmert", "contr.poly"))
     lm2 <- lm(dv ~ a*b, data = fidler_thompson)
     t2 <- apa.aov.table(lm2)
     predictors <- t2$table_body$Predictor
     expect_true("a" %in% predictors)
     expect_true("b" %in% predictors)
     expect_true("a x b" %in% predictors)
     expect_true("Error" %in% predictors)
     expect_equal(nrow(t2$table_body), 5)
})

test_that("apa.aov.table 2-way goggles has correct predictors", {
     options(contrasts = c("contr.helmert", "contr.poly"))
     lm3 <- lm(attractiveness ~ gender*alcohol, data = goggles)
     t3 <- apa.aov.table(lm3)
     predictors <- t3$table_body$Predictor
     expect_true("gender" %in% predictors)
     expect_true("alcohol" %in% predictors)
     expect_true("gender x alcohol" %in% predictors)
})

test_that("apa.aov.table conf.level parameter changes CI labels", {
     options(contrasts = c("contr.helmert", "contr.poly"))
     lm1 <- lm(libido ~ dose, data = viagra)
     t90 <- apa.aov.table(lm1, conf.level = .90)
     t95 <- apa.aov.table(lm1, conf.level = .95)
     expect_true("CI_90_partial_eta2" %in% names(t90$table_body))
     expect_true("CI_95_partial_eta2" %in% names(t95$table_body))
})

test_that("apa.aov.table table.number parameter works", {
     options(contrasts = c("contr.helmert", "contr.poly"))
     lm1 <- lm(libido ~ dose, data = viagra)
     t1 <- apa.aov.table(lm1, table.number = 9)
     expect_equal(t1$table_number, 9)
})

test_that("apa.aov.table has LaTeX components", {
     options(contrasts = c("contr.helmert", "contr.poly"))
     lm1 <- lm(libido ~ dose, data = viagra)
     t1 <- apa.aov.table(lm1)
     expect_false(is.null(t1$latex.body))
     expect_false(is.null(t1$latex.table.note))
})

test_that("apa.aov.table title mentions DV name", {
     options(contrasts = c("contr.helmert", "contr.poly"))
     lm1 <- lm(libido ~ dose, data = viagra)
     t1 <- apa.aov.table(lm1)
     expect_true(grepl("Libido", t1$table_title))
})

test_that("apa.aov.table filename defaults to NA like every other builder (docs audit)", {
     # aov (and afex) used to declare bare `filename` with a missing()
     # guard -- optional in practice, required in the printed signature.
     # Normalized 2026-08-09: default NA, is.na() guard, same as the
     # other five builders. Explicit NA must mean "no file" too, which
     # the old missing() guard got wrong.
     options(contrasts = c("contr.helmert", "contr.poly"))
     lm1 <- lm(libido ~ dose, data = viagra)
     expect_identical(formals(apa.aov.table)$filename, NA)
     withr::with_tempdir({
          t_default <- apa.aov.table(lm1)
          t_na <- apa.aov.table(lm1, filename = NA)
          expect_equal(length(list.files()), 0)
          expect_equal(t_na$table_body, t_default$table_body)
          apa.aov.table(lm1, filename = "aov.docx")
          expect_true(file.exists("aov.docx"))
     })
})

aov_row <- function(tbl, predictor) {
     tbl$table_body[tbl$table_body$Predictor == predictor, , drop = FALSE]
}

test_that("type = 2 gives every effect row its confidence interval", {
     # The interval loop used to start at row 2, assuming row 1 was the
     # (Intercept) that only Type III has, so the first effect lost its CI.
     withr::local_options(contrasts = c("contr.sum", "contr.poly"))
     t2 <- apa.aov.table(lm(attractiveness ~ gender * alcohol, data = goggles),
                         type = 2)
     ci <- t2$table_body$CI_90_partial_eta2
     effects <- t2$table_body$Predictor != "Error"
     expect_false("(Intercept)" %in% t2$table_body$Predictor)
     expect_true(all(grepl("^\\[", ci[effects])))
     expect_identical(aov_row(t2, "gender")$CI_90_partial_eta2, "[.00, .18]")
})

test_that("rescaling the DV leaves F, p and partial eta-squared unchanged", {
     # SS used to be rounded to 2 dp before partial eta-squared was
     # computed, so a small-scale DV gave 0/0 = NaN.
     withr::local_options(contrasts = c("contr.sum", "contr.poly"))
     small <- goggles
     small$attractiveness <- small$attractiveness / 1000
     ref <- apa.aov.table(lm(attractiveness ~ gender * alcohol, data = goggles))
     tiny <- apa.aov.table(lm(attractiveness ~ gender * alcohol, data = small))
     cols <- c("Predictor", "df", "F", "p", "partial_eta2", "CI_90_partial_eta2")
     expect_identical(tiny$table_body[cols], ref$table_body[cols])
     expect_false(any(grepl("NaN", unlist(tiny$table_body))))
     expect_false(any(grepl("NaN", unlist(tiny$spec$body))))
})

test_that("MS is computed from the unrounded SS", {
     # dose SS is 20.1333; 20.1333 / 2 = 10.067, which the old code printed
     # as 10.06 by dividing the already-rounded 20.13.
     withr::local_options(contrasts = c("contr.sum", "contr.poly"))
     t1 <- apa.aov.table(lm(libido ~ dose, data = viagra))
     expect_identical(aov_row(t1, "dose")$MS, "10.07")
})

test_that("Type III gives main effects whatever contrasts the model was fitted with", {
     # Under R's default treatment contrasts a factor's Type III row in a
     # model with an interaction is a simple effect at the reference level
     # (alcohol F = 0.61, p = .546). apa.aov.table refits with sum-to-zero
     # contrasts, so the table matches the one made with them set.
     withr::local_options(contrasts = c("contr.sum", "contr.poly"))
     ref <- apa.aov.table(lm(attractiveness ~ gender * alcohol, data = goggles))
     withr::local_options(contrasts = c("contr.treatment", "contr.poly"))
     m <- lm(attractiveness ~ gender * alcohol, data = goggles)
     expect_identical(m$contrasts$alcohol, "contr.treatment")
     t3 <- apa.aov.table(m)
     expect_identical(t3$table_body, ref$table_body)
     expect_identical(aov_row(t3, "alcohol")$F, "20.07")
     expect_identical(aov_row(t3, "alcohol")$p, "<.001")
})

test_that("the refit finds data that exists only inside a function", {
     withr::local_options(contrasts = c("contr.sum", "contr.poly"))
     ref <- apa.aov.table(lm(attractiveness ~ gender * alcohol, data = goggles))
     withr::local_options(contrasts = c("contr.treatment", "contr.poly"))
     fit_locally <- function() {
          local_data <- goggles
          lm(attractiveness ~ gender * alcohol, data = local_data)
     }
     expect_identical(apa.aov.table(fit_locally())$table_body, ref$table_body)
})

test_that("a Type II table does not depend on the contrasts", {
     withr::local_options(contrasts = c("contr.sum", "contr.poly"))
     ref <- apa.aov.table(lm(attractiveness ~ gender * alcohol, data = goggles),
                          type = 2)
     withr::local_options(contrasts = c("contr.treatment", "contr.poly"))
     t2 <- apa.aov.table(lm(attractiveness ~ gender * alcohol, data = goggles),
                         type = 2)
     # The interaction SS is exactly 1978.125, so the last printed digit is
     # a floating-point coin flip between codings; compare SS and MS as
     # numbers, everything else as printed.
     cols <- c("Predictor", "df", "F", "p", "partial_eta2", "CI_90_partial_eta2")
     expect_identical(t2$table_body[cols], ref$table_body[cols])
     ss_gap <- abs(as.numeric(t2$table_body$SS) - as.numeric(ref$table_body$SS))
     expect_true(all(ss_gap < 0.011))
})

test_that("only Type III is refitted, whichever way the type is written", {
     # Type II sums of squares do not depend on the coding, so a refit there
     # cannot change the table -- watch whether the refit is called instead.
     withr::local_options(contrasts = c("contr.treatment", "contr.poly"))
     m <- lm(attractiveness ~ gender * alcohol, data = goggles)
     calls <- 0
     local_mocked_bindings(aov_sum_contrasts = function(lm_output) {
          calls <<- calls + 1
          lm_output
     })
     for (type in list(2, 2L, "2", "II")) apa.aov.table(m, type = type)
     expect_identical(calls, 0)
     for (type in list(3, 3L, "3", "III")) apa.aov.table(m, type = type)
     expect_identical(calls, 4)
})

test_that("type = \"III\" gives main effects under treatment contrasts", {
     withr::local_options(contrasts = c("contr.treatment", "contr.poly"))
     t3 <- apa.aov.table(lm(attractiveness ~ gender * alcohol, data = goggles),
                         type = "III")
     expect_identical(aov_row(t3, "alcohol")$F, "20.07")
})

test_that("the refit uses the fitted model, not the workspace as it is now", {
     # The refit is built from the model's stored frame and weights.
     # Re-running the model's call instead would pick up whatever the
     # workspace holds now: here, weights reused for another analysis.
     withr::local_options(contrasts = c("contr.sum", "contr.poly"))
     d <- goggles
     w <- 1 / stats::ave(d$attractiveness, d$gender, d$alcohol, FUN = stats::var)
     ref <- apa.aov.table(lm(attractiveness ~ gender * alcohol, data = d, weights = w))
     withr::local_options(contrasts = c("contr.treatment", "contr.poly"))
     m <- lm(attractiveness ~ gender * alcohol, data = d, weights = w)
     w <- rep(1, nrow(d))
     d$attractiveness <- rev(d$attractiveness)
     expect_identical(apa.aov.table(m)$table_body, ref$table_body)
     expect_identical(aov_row(ref, "alcohol")$F, "20.05")
})

test_that("models whose call can no longer be run are still refitted", {
     withr::local_options(contrasts = c("contr.sum", "contr.poly"))
     ref <- apa.aov.table(lm(attractiveness ~ gender * alcohol, data = goggles))
     withr::local_options(contrasts = c("contr.treatment", "contr.poly"))
     # A formula variable reused by a loop, and a wrapper whose stored call
     # reads lm(formula = ..1, data = ..2).
     g <- goggles
     g$other <- g$attractiveness + 1
     models <- list()
     for (dv in c("attractiveness", "other")) {
          f <- stats::as.formula(paste(dv, "~ gender * alcohol"))
          models[[dv]] <- lm(f, data = g)
     }
     expect_identical(apa.aov.table(models[["attractiveness"]])$table_body,
                      ref$table_body)
     fit <- function(...) lm(...)
     expect_identical(apa.aov.table(fit(attractiveness ~ gender * alcohol,
                                        data = goggles))$table_body,
                      ref$table_body)
})

test_that("a model stored without its data is refused with the line to add", {
     withr::local_options(contrasts = c("contr.treatment", "contr.poly"))
     m <- lm(attractiveness ~ gender * alcohol, data = goggles)
     m$model <- NULL
     expect_error(aov_sum_contrasts(m), "options\\(contrasts = c\\(\"contr.sum\"")
})

test_that("the CI comes from the unrounded F", {
     # In this subset the interaction's F is 8.0559. Its lower bound is .0950
     # from that F but .0951 from F rounded to 8.06 -- [.09, .47] against
     # [.10, .47].
     withr::local_options(contrasts = c("contr.sum", "contr.poly"))
     d <- goggles[-c(5, 12, 17, 31, 36, 40, 41, 42), ]
     t1 <- apa.aov.table(lm(attractiveness ~ gender * alcohol, data = d))
     expect_identical(aov_row(t1, "gender x alcohol")$CI_90_partial_eta2,
                      "[.09, .47]")
})

test_that("a model already fitted with sum-to-zero contrasts is used as it is", {
     withr::local_options(contrasts = c("contr.helmert", "contr.poly"))
     m <- lm(attractiveness ~ gender * alcohol, data = goggles)
     expect_identical(aov_sum_contrasts(m), m)
})

test_that("a model with an offset is refused, in either form", {
     msg <- "does not support models with an offset"
     expect_error(apa.aov.table(lm(attractiveness ~ gender * alcohol + offset(rep(1, 48)),
                                   data = goggles)), msg)
     expect_error(apa.aov.table(lm(attractiveness ~ gender * alcohol, data = goggles,
                                   offset = rep(1, 48))), msg)
     expect_error(apa.aov.table(lm(attractiveness ~ gender, data = goggles,
                                   offset = rep(1, 48)), type = 2), msg)
})
