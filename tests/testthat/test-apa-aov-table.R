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
