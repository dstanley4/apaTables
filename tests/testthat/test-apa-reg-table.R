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

test_that("blocks with different weights are refused", {
     expect_error(apa.reg.table(lm(mpg ~ wt, data = mtcars, weights = cyl),
                                lm(mpg ~ wt + qsec, data = mtcars)),
                  "same weights")
})

test_that("a model with an offset is refused, in either form and any block", {
     msg <- "does not support models with an offset"
     expect_error(apa.reg.table(lm(mpg ~ wt + offset(hp / 20), data = mtcars)), msg)
     expect_error(apa.reg.table(lm(mpg ~ wt, data = mtcars, offset = hp / 20)), msg)
     expect_error(apa.reg.table(lm(mpg ~ wt, data = mtcars),
                                lm(mpg ~ wt + qsec + offset(hp / 20), data = mtcars)), msg)
     # stored without its model frame, the offset is still seen
     expect_error(apa.reg.table(lm(mpg ~ wt + offset(hp / 20), data = mtcars, model = FALSE)), msg)
     # a wrapper whose offset argument is NULL fits no offset, and is accepted
     fit <- function(o = NULL) lm(mpg ~ wt, data = mtcars, offset = o)
     expect_s3_class(apa.reg.table(fit()), "apa_table")
     withr::local_options(contrasts = c("contr.sum", "contr.poly"))
     fit_aov <- function(o = NULL) lm(mpg ~ factor(cyl), data = mtcars, offset = o)
     expect_s3_class(apa.aov.table(fit_aov()), "apa_table")
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

test_that("a predictor name with a hyphen or a space keeps beta and r", {
     d <- mtcars
     d$`pre-test` <- d$wt
     d$`my var` <- d$qsec
     shown <- c("b", "b_95%_CI", "beta", "beta_95%_CI", "Unique_R2", "r")
     plain <- apa.reg.table(lm(mpg ~ wt + qsec, data = d))
     odd   <- apa.reg.table(lm(mpg ~ `pre-test` + `my var`, data = d))
     expect_identical(odd$table_body[, shown], plain$table_body[, shown])
     expect_identical(odd$table_body$Predictor[2:3], c("pre-test", "my var"))
     expect_match(odd$table_note, "zero-order correlation")
     # the same holds in blocks
     plain2 <- apa.reg.table(lm(mpg ~ wt, data = d), lm(mpg ~ wt + qsec, data = d))
     odd2   <- apa.reg.table(lm(mpg ~ `pre-test`, data = d),
                             lm(mpg ~ `pre-test` + `my var`, data = d))
     expect_identical(odd2$table_body[, shown], plain2$table_body[, shown])
     expect_identical(odd2$table_body$Predictor[c(3, 9, 10)],
                      c("pre-test", "pre-test", "my var"))
     # a term built around such a name keeps them too
     expect_identical(apa.reg.table(lm(mpg ~ log(`pre-test`) + qsec, data = d))$table_body[, shown],
                      apa.reg.table(lm(mpg ~ log(wt) + qsec, data = d))$table_body[, shown])
})

test_that("r is left blank only for a term that multiplies", {
     d <- mtcars
     d$`a*b` <- d$hp
     d$`pre-test` <- d$wt
     shown <- c("b", "beta", "Unique_R2", "r")
     # a variable named `a*b` is not a product
     expect_identical(apa.reg.table(lm(mpg ~ `a*b` + qsec, data = d))$table_body[, shown],
                      apa.reg.table(lm(mpg ~ hp + qsec, data = d))$table_body[, shown])
     # nor is a sum inside I()
     expect_identical(apa.reg.table(lm(mpg ~ wt + I(`a*b` + qsec), data = d))$table_body[, shown],
                      apa.reg.table(lm(mpg ~ wt + I(hp + qsec), data = d))$table_body[, shown])
     # a product, with an ordinary or a backticked name, has a blank r
     prod1 <- apa.reg.table(lm(sales ~ adverts + airplay + I(adverts * airplay), data = album))
     expect_identical(prod1$table_body$r[2:4], c(".58**", ".60**", ""))   # cor.test: .5785, .5989
     prod2 <- apa.reg.table(lm(mpg ~ `pre-test` + qsec + I(`pre-test` * qsec), data = d))
     expect_identical(prod2$table_body$r[2:4], c("-.87**", ".42*", ""))   # cor.test: -.8677, .4187 (p = .017)
})

test_that("each row label is the term as summary() prints it", {
     # factor rows are named by level under R's default contrasts; other
     # test files set sum-to-zero contrasts, which name them 1, 2, ...
     withr::local_options(contrasts = c("contr.treatment", "contr.poly"))
     labels <- function(f, d = mtcars) {
          p <- apa.reg.table(lm(f, data = d))$table_body$Predictor
          p[!p %in% c("", "(Intercept)")]
     }
     expect_identical(labels(mpg ~ log(wt) + sqrt(hp) + scale(qsec)),
                      c("log(wt)", "sqrt(hp)", "scale(qsec)"))
     expect_identical(labels(mpg ~ factor(cyl) + poly(wt, 2)),
                      c("factor(cyl)6", "factor(cyl)8", "poly(wt, 2)1", "poly(wt, 2)2"))
     expect_identical(labels(mpg ~ wt * qsec), c("wt", "qsec", "wt:qsec"))
     # the three changes from summary(): a product reads "a x b", the I( )
     # wrapper is dropped, and names have no backticks
     expect_identical(labels(mpg ~ wt + qsec + I(wt * qsec) + I(wt^2)),
                      c("wt", "qsec", "wt x qsec", "wt^2"))
     d <- mtcars
     d$`pre-test` <- d$wt
     expect_identical(labels(mpg ~ log(`pre-test`) + qsec + I(`pre-test` * qsec), d),
                      c("log(pre-test)", "qsec", "pre-test x qsec"))
     # a factor level keeps the text R gives it, even when that text could be
     # read as R code: 26-35 is an age band, not a subtraction, and # is
     # not a comment
     d$agegrp <- factor(rep(c("18-25", "26-35", "36-45", "46-55"), 8))
     d$site <- factor(rep(c("Site #1", "Site #2", "Site #3", "Site #4"), 8))
     expect_identical(labels(mpg ~ agegrp + wt, d),
                      c("agegrp26-35", "agegrp36-45", "agegrp46-55", "wt"))
     expect_identical(labels(mpg ~ site + wt, d),
                      c("siteSite #2", "siteSite #3", "siteSite #4", "wt"))
     expect_identical(labels(mpg ~ I(cyl == 4) + wt), c("I(cyl == 4)TRUE", "wt"))
     # a factor on a name with a space: the level label has no backticks
     d$`my group` <- factor(rep(c("A", "B"), 16))
     expect_identical(labels(mpg ~ `my group` * qsec, d),
                      c("my groupB", "qsec", "my groupB:qsec"))
     # block labels are untouched
     t2 <- apa.reg.table(lm(mpg ~ log(wt), data = mtcars), lm(mpg ~ log(wt) + hp, data = mtcars))
     expect_identical(t2$table_body$Predictor[c(1, 3, 7, 9, 10)],
                      c("Model 1", "log(wt)", "Model 2", "log(wt)", "hp"))
})

test_that("beta and r are left out of every block when any block cannot show them", {
     no_beta_r <- function(t1) {
          expect_false(any(c("beta", "beta_95%_CI", "r") %in% names(t1$table_body)))
          expect_false(grepl("zero-order", t1$table_note))
     }
     # the first block cannot show them, a later block can
     t1 <- apa.reg.table(lm(mpg ~ wt * qsec, data = mtcars),
                         lm(mpg ~ wt + qsec + I(wt * qsec) + hp, data = mtcars))
     no_beta_r(t1)
     expect_identical(t1$table_body$Predictor[c(1, 5, 9, 13, 14)],
                      c("Model 1", "wt:qsec", "Model 2", "wt x qsec", "hp"))
     # the middle block of three cannot show them
     d <- transform(mtcars, f = factor(gear))
     no_beta_r(apa.reg.table(lm(mpg ~ wt, data = d), lm(mpg ~ wt + f, data = d),
                             lm(mpg ~ wt + f + hp, data = d)))
     # a later block cannot show them (the direction that already worked)
     no_beta_r(apa.reg.table(lm(mpg ~ wt, data = d), lm(mpg ~ wt + f, data = d)))
     # b and the fit statistics are what each model gives on its own
     blk2 <- lm(mpg ~ wt + qsec + I(wt * qsec) + hp, data = mtcars)
     expect_true(startsWith(t1$table_body$b[14], sprintf("%1.2f", coef(blk2)[["hp"]])))
})

test_that("a session sum-contrasts line does not change a regression table", {
     d <- transform(mtcars, cyl = factor(cyl), ch = ifelse(am == 1, "manual", "auto"),
                    gr = factor(gear, ordered = TRUE))
     d$w <- rep(1:2, 16)
     tables <- function() list(
          apa.reg.table(lm(mpg ~ cyl + wt, data = d)),
          apa.reg.table(lm(mpg ~ cyl * wt, data = d)),
          apa.reg.table(lm(mpg ~ wt, data = d), lm(mpg ~ wt + cyl, data = d),
                        lm(mpg ~ wt + cyl + hp, data = d)),
          apa.reg.table(lm(mpg ~ cyl + ch + gr + wt, data = d, weights = w)))
     withr::local_options(contrasts = c("contr.treatment", "contr.poly"))
     default <- tables()
     withr::local_options(contrasts = c("contr.sum", "contr.poly"))
     habit <- tables()
     for (i in seq_along(default)) {
          expect_identical(habit[[i]]$table_body, default[[i]]$table_body)
          expect_identical(habit[[i]]$spec, default[[i]]$spec)
     }
     # an ordered factor keeps the polynomial rows R gives it
     expect_true(all(c("gr.L", "gr.Q") %in% default[[4]]$table_body$Predictor))
     # the default-coding values, checked against lm() itself
     ref <- coef(lm(mpg ~ cyl + wt, data = d, contrasts = list(cyl = "contr.treatment")))
     b <- default[[1]]$table_body
     expect_identical(b$Predictor[2:3], c("cyl6", "cyl8"))
     expect_true(startsWith(b$b[2], sprintf("%1.2f", ref[["cyl6"]])))   # -4.26
})

test_that("coding chosen for one model is kept if dummy coding and refused otherwise", {
     withr::local_options(contrasts = c("contr.treatment", "contr.poly"))
     d <- transform(mtcars, cyl = factor(cyl))
     expect_error(apa.reg.table(lm(mpg ~ cyl + wt, data = d, contrasts = list(cyl = "contr.sum"))),
                  "coded another way")
     d2 <- d
     contrasts(d2$cyl) <- contr.sum(3)
     expect_error(apa.reg.table(lm(mpg ~ cyl + wt, data = d2)), "coded another way")
     # dummy coding with another reference level (8 cylinders) is kept as fitted
     sas <- apa.reg.table(lm(mpg ~ cyl + wt, data = d, contrasts = list(cyl = "contr.SAS")))
     expect_identical(sas$table_body$Predictor[2:3], c("cyl4", "cyl6"))
     # C() in the formula and a coding name set on the variable are choices too
     expect_error(apa.reg.table(lm(mpg ~ C(cyl, sum) + wt, data = d)), "remove C\\(\\)")
     d3 <- d
     contrasts(d3$cyl) <- "contr.sum"
     expect_error(apa.reg.table(lm(mpg ~ cyl + wt, data = d3)), "coded another way")
     # dummy coding without level names cannot label the rows
     d4 <- d
     contrasts(d4$cyl) <- contr.treatment(3)
     expect_error(apa.reg.table(lm(mpg ~ cyl + wt, data = d4)), "not named by level")
     # a contrasts list given as an object cannot be read, so the session
     # coding beside it cannot be told apart from a choice
     withr::local_options(contrasts = c("contr.sum", "contr.poly"))
     d5 <- transform(d, am = factor(am))
     my_ctr <- list(cyl = "contr.treatment")
     expect_error(apa.reg.table(lm(mpg ~ cyl + am + wt, data = d5, contrasts = my_ctr)),
                  "given as an object")
     # written inside lm(), the list is read: cyl kept, am refitted
     t5 <- apa.reg.table(lm(mpg ~ cyl + am + wt, data = d5, contrasts = list(cyl = "contr.treatment")))
     expect_identical(t5$table_body$Predictor[2:4], c("cyl6", "cyl8", "am1"))
     # a model stored without its data cannot be used at all
     expect_error(apa.reg.table(lm(mpg ~ cyl + wt, data = d, model = FALSE)), "model = FALSE")
})

