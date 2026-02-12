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

test_that("apa.afex.table has RTF and LaTeX components", {
     skip_if_not_installed("afex")
     goggles_results <- suppressMessages(make_goggles_afex())
     t1 <- apa.afex.table(goggles_results)
     expect_false(is.null(t1$rtf.body))
     expect_false(is.null(t1$latex.body))
     expect_false(is.null(t1$rtf.table.title))
     expect_false(is.null(t1$latex.table.note))
     expect_true(t1$landscape)
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
     tmp_file <- file.path(tempdir(), "test_afex_table.rtf")
     t1 <- apa.afex.table(goggles_results, filename = tmp_file)
     expect_true(file.exists(tmp_file))
     unlink(tmp_file)
})
