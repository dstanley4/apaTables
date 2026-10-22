library(apaTables)

test_that("apa.reg.table single block returns correct class and type", {
     blk1 <- lm(sales ~ adverts + airplay + attract, data = album)
     r1 <- apa.reg.table(blk1, table.number = 1)
     expect_s3_class(r1, "apa_table")
     expect_equal(r1$table.type, "regression")
})

test_that("apa.reg.table single block has correct structure", {
     blk1 <- lm(sales ~ adverts + airplay + attract, data = album)
     r1 <- apa.reg.table(blk1, table.number = 1)
     expect_equal(r1$table_number, 1)
     expect_true(is.data.frame(r1$table_body))
     expect_equal(nrow(r1$table_body), 7)
     # Single block should not have Delta_Fit column
     expect_false("Delta_Fit" %in% names(r1$table_body))
     expect_true("Fit" %in% names(r1$table_body))
})

test_that("apa.reg.table single block has correct predictors", {
     blk1 <- lm(sales ~ adverts + airplay + attract, data = album)
     r1 <- apa.reg.table(blk1)
     predictors <- r1$table_body$Predictor
     expect_true("(Intercept)" %in% predictors)
     expect_true("adverts" %in% predictors)
     expect_true("airplay" %in% predictors)
     expect_true("attract" %in% predictors)
})

test_that("apa.reg.table single block title says Regression", {
     blk1 <- lm(sales ~ adverts + airplay + attract, data = album)
     r1 <- apa.reg.table(blk1)
     expect_true(grepl("Regression", r1$table_title))
     expect_true(grepl("Sales", r1$table_title))
     expect_false(grepl("Hierarchical", r1$table_title))
})

test_that("apa.reg.table single block column names at 95% default", {
     blk1 <- lm(sales ~ adverts + airplay + attract, data = album)
     r1 <- apa.reg.table(blk1)
     col_names <- names(r1$table_body)
     expect_true("Predictor" %in% col_names)
     expect_true("b" %in% col_names)
     expect_true("b_95%_CI" %in% col_names)
     expect_true("beta" %in% col_names)
     expect_true("Unique_R2" %in% col_names)
     expect_true("Unique_95%_CI" %in% col_names)
     expect_true("r" %in% col_names)
})

test_that("apa.reg.table two blocks has correct structure", {
     blk1 <- lm(sales ~ adverts, data = album)
     blk2 <- lm(sales ~ adverts + airplay + attract, data = album)
     r2 <- apa.reg.table(blk1, blk2, table.number = 2)
     expect_s3_class(r2, "apa_table")
     expect_equal(r2$table_number, 2)
     expect_true("Delta_Fit" %in% names(r2$table_body))
     expect_true(grepl("Hierarchical", r2$table_title))
     expect_equal(length(r2$table_block_results), 2)
})

test_that("apa.reg.table two blocks has Model labels", {
     blk1 <- lm(sales ~ adverts, data = album)
     blk2 <- lm(sales ~ adverts + airplay + attract, data = album)
     r2 <- apa.reg.table(blk1, blk2)
     predictors <- r2$table_body$Predictor
     expect_true("Model 1" %in% predictors)
     expect_true("Model 2" %in% predictors)
})

test_that("apa.reg.table table.number parameter works", {
     blk1 <- lm(sales ~ adverts + airplay + attract, data = album)
     r1 <- apa.reg.table(blk1, table.number = 7)
     expect_equal(r1$table_number, 7)
})

test_that("apa.reg.table prop.var.conf.level changes CI labels", {
     blk1 <- lm(sales ~ adverts + airplay + attract, data = album)
     r95 <- apa.reg.table(blk1, prop.var.conf.level = .95)
     r90 <- apa.reg.table(blk1, prop.var.conf.level = .90)
     expect_true("Unique_95%_CI" %in% names(r95$table_body))
     expect_true("Unique_90%_CI" %in% names(r90$table_body))
})

test_that("apa.reg.table single predictor model works", {
     blk1 <- lm(sales ~ adverts, data = album)
     r1 <- apa.reg.table(blk1)
     expect_s3_class(r1, "apa_table")
     expect_true("(Intercept)" %in% r1$table_body$Predictor)
     expect_true("adverts" %in% r1$table_body$Predictor)
})

test_that("apa.reg.table interaction terms work", {
     blk1 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data = album)
     r1 <- apa.reg.table(blk1)
     expect_s3_class(r1, "apa_table")
     expect_true("adverts x airplay" %in% r1$table_body$Predictor)
})

test_that("apa.reg.table has LaTeX components", {
     blk1 <- lm(sales ~ adverts + airplay + attract, data = album)
     r1 <- apa.reg.table(blk1)
     expect_false(is.null(r1$latex.body))
     expect_false(is.null(r1$latex.table.title))
     expect_true(is.na(r1$landscape))
})

test_that("blocks that share no predictor are refused", {
     # The old check compared intersect() with the previous block, which is
     # vacuously TRUE for disjoint blocks, and printed Delta R2 = -.150 with
     # a CI of [NaN, NaN] under a hierarchical title.
     expect_error(apa.reg.table(lm(mpg ~ wt, data = mtcars),
                                lm(mpg ~ hp, data = mtcars)),
                  "must contain every term of the preceding block")
})

test_that("a block that adds no predictor is refused", {
     expect_error(apa.reg.table(lm(mpg ~ wt, data = mtcars),
                                lm(mpg ~ wt, data = mtcars)),
                  "plus at least one more")
})

test_that("a block that drops an interaction is refused", {
     expect_error(apa.reg.table(lm(mpg ~ wt * hp, data = mtcars),
                                lm(mpg ~ wt + hp + qsec, data = mtcars)),
                  "including any interaction")
})

test_that("a later block that adds predictors but drops one is refused", {
     # Block 2 has more columns than block 1, so only the nesting check (does
     # block 2 contain everything block 1 had?) can refuse it.
     expect_error(apa.reg.table(lm(mpg ~ wt + hp, data = mtcars),
                                lm(mpg ~ wt + qsec + drat, data = mtcars)),
                  "must contain every term of the preceding block")
})

test_that("a step written as a new parameterisation is accepted", {
     # poly(wt, 2) spans wt, so this is a valid hierarchical step, and its
     # Delta R2 is the same as for wt then wt + I(wt^2).
     t_poly <- apa.reg.table(lm(mpg ~ wt, data = mtcars),
                             lm(mpg ~ poly(wt, 2), data = mtcars))
     t_raw <- apa.reg.table(lm(mpg ~ wt, data = mtcars),
                            lm(mpg ~ wt + I(wt^2), data = mtcars))
     delta <- function(t) t$table_body$Delta_Fit[grepl("Delta", t$table_body$Delta_Fit)]
     expect_identical(delta(t_poly), delta(t_raw))
})

test_that("blocks with different weights or offsets are refused", {
     expect_error(apa.reg.table(lm(mpg ~ wt, data = mtcars, weights = cyl),
                                lm(mpg ~ wt + qsec, data = mtcars)),
                  "same weights and the same offset")
     expect_error(apa.reg.table(lm(mpg ~ wt + offset(hp / 20), data = mtcars),
                                lm(mpg ~ wt + qsec, data = mtcars)),
                  "same weights and the same offset")
})

test_that("blocks fitted to different cases are refused with a named message", {
     incomplete <- mtcars
     incomplete$hp[1:5] <- NA
     expect_error(apa.reg.table(lm(mpg ~ wt, data = incomplete),
                                lm(mpg ~ wt + hp, data = incomplete)),
                  "must be fitted to the same cases")
     # The same cases in a different order cannot be compared row by row.
     shuffled <- mtcars[rev(seq_len(nrow(mtcars))), ]
     expect_error(apa.reg.table(lm(mpg ~ wt, data = mtcars),
                                lm(mpg ~ wt + hp, data = shuffled)),
                  "in a different order")
})

test_that("an interaction written in either order counts as the same term", {
     t1 <- apa.reg.table(lm(mpg ~ wt * hp, data = mtcars),
                         lm(mpg ~ hp * wt + cyl, data = mtcars))
     expect_s3_class(t1, "apa_table")
})
