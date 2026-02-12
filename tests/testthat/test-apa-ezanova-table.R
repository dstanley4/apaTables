library(apaTables)

test_that("apa.ezANOVA.table works with between-subjects design (goggles)", {
     skip_if_not_installed("ez")
     options(digits = 10)
     goggles_results <- ez::ezANOVA(data = goggles,
                                     dv = attractiveness,
                                     between = .(gender, alcohol),
                                     wid = participant,
                                     detailed = TRUE)
     t1 <- apa.ezANOVA.table(goggles_results)
     expect_s3_class(t1, "apa_table")
     expect_equal(t1$table.type, "ezanova")
})

test_that("apa.ezANOVA.table between-subjects has correct predictors", {
     skip_if_not_installed("ez")
     options(digits = 10)
     goggles_results <- ez::ezANOVA(data = goggles,
                                     dv = attractiveness,
                                     between = .(gender, alcohol),
                                     wid = participant,
                                     detailed = TRUE)
     t1 <- apa.ezANOVA.table(goggles_results)
     predictors <- t1$table_body$Predictor
     expect_true("gender" %in% predictors)
     expect_true("alcohol" %in% predictors)
     expect_true("gender x alcohol" %in% predictors)
})

test_that("apa.ezANOVA.table between-subjects has expected columns", {
     skip_if_not_installed("ez")
     options(digits = 10)
     goggles_results <- ez::ezANOVA(data = goggles,
                                     dv = attractiveness,
                                     between = .(gender, alcohol),
                                     wid = participant,
                                     detailed = TRUE)
     t1 <- apa.ezANOVA.table(goggles_results)
     col_names <- names(t1$table_body)
     expect_true("Predictor" %in% col_names)
     expect_true("F" %in% col_names)
     expect_true("p" %in% col_names)
     expect_true("ges" %in% col_names)
})

test_that("apa.ezANOVA.table table.number and table.title parameters work", {
     skip_if_not_installed("ez")
     options(digits = 10)
     goggles_results <- ez::ezANOVA(data = goggles,
                                     dv = attractiveness,
                                     between = .(gender, alcohol),
                                     wid = participant,
                                     detailed = TRUE)
     t1 <- apa.ezANOVA.table(goggles_results, table.number = 3,
                              table.title = "Custom ANOVA Title")
     expect_equal(t1$table_number, 3)
     expect_equal(t1$table_title, "Custom ANOVA Title")
})

test_that("apa.ezANOVA.table works with within-subjects design", {
     skip_if_not_installed("ez")
     skip_if_not_installed("tidyr")
     options(digits = 10)

     drink_attitude_long <- tidyr::pivot_longer(drink_attitude_wide,
                                                 cols = beer_positive:water_neutral,
                                                 names_to = c("drink", "imagery"),
                                                 names_sep = "_",
                                                 values_to = "attitude")
     drink_attitude_long$drink <- as.factor(drink_attitude_long$drink)
     drink_attitude_long$imagery <- as.factor(drink_attitude_long$imagery)

     drink_attitude_results <- ez::ezANOVA(data = drink_attitude_long,
                                            dv = .(attitude), wid = .(participant),
                                            within = .(drink, imagery),
                                            type = 3, detailed = TRUE)

     t1 <- apa.ezANOVA.table(drink_attitude_results)
     expect_s3_class(t1, "apa_table")
     expect_equal(t1$table.type, "ezanova")
})

test_that("apa.ezANOVA.table within-subjects has correct effects", {
     skip_if_not_installed("ez")
     skip_if_not_installed("tidyr")
     options(digits = 10)

     drink_attitude_long <- tidyr::pivot_longer(drink_attitude_wide,
                                                 cols = beer_positive:water_neutral,
                                                 names_to = c("drink", "imagery"),
                                                 names_sep = "_",
                                                 values_to = "attitude")
     drink_attitude_long$drink <- as.factor(drink_attitude_long$drink)
     drink_attitude_long$imagery <- as.factor(drink_attitude_long$imagery)

     drink_attitude_results <- ez::ezANOVA(data = drink_attitude_long,
                                            dv = .(attitude), wid = .(participant),
                                            within = .(drink, imagery),
                                            type = 3, detailed = TRUE)

     t1 <- apa.ezANOVA.table(drink_attitude_results)
     predictors <- t1$table_body$Predictor
     expect_true("drink" %in% predictors)
     expect_true("imagery" %in% predictors)
     expect_true("drink x imagery" %in% predictors)
})

test_that("apa.ezANOVA.table has RTF and LaTeX components", {
     skip_if_not_installed("ez")
     options(digits = 10)
     goggles_results <- ez::ezANOVA(data = goggles,
                                     dv = attractiveness,
                                     between = .(gender, alcohol),
                                     wid = participant,
                                     detailed = TRUE)
     t1 <- apa.ezANOVA.table(goggles_results)
     expect_false(is.null(t1$rtf.body))
     expect_false(is.null(t1$latex.body))
     expect_false(is.null(t1$rtf.table.title))
     expect_false(is.null(t1$latex.table.note))
     expect_true(t1$landscape)
})

test_that("apa.ezANOVA.table non-detailed between-subjects works", {
     skip_if_not_installed("ez")
     options(digits = 10)
     goggles_results <- ez::ezANOVA(data = goggles,
                                     dv = attractiveness,
                                     between = .(gender, alcohol),
                                     wid = participant,
                                     detailed = FALSE)
     t1 <- apa.ezANOVA.table(goggles_results)
     expect_s3_class(t1, "apa_table")
     col_names <- names(t1$table_body)
     expect_false("SS_num" %in% col_names)
     expect_false("SS_den" %in% col_names)
})
