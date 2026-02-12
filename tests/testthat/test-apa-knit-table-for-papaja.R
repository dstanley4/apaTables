library(apaTables)

test_that("apa.knit.table.for.papaja works with regression table", {
     skip_if_not_installed("papaja")
     blk1 <- lm(sales ~ adverts + airplay + attract, data = album)
     r1 <- apa.reg.table(blk1)
     out <- apa.knit.table.for.papaja(r1)
     expect_true(nchar(out) > 0)
})

test_that("apa.knit.table.for.papaja works with hierarchical regression", {
     skip_if_not_installed("papaja")
     blk1 <- lm(sales ~ adverts, data = album)
     blk2 <- lm(sales ~ adverts + airplay + attract, data = album)
     r2 <- apa.reg.table(blk1, blk2)
     out <- apa.knit.table.for.papaja(r2)
     expect_true(nchar(out) > 0)
})

test_that("apa.knit.table.for.papaja returns blank for non-regression", {
     skip_if_not_installed("papaja")
     t1 <- apa.cor.table(attitude)
     out <- apa.knit.table.for.papaja(t1)
     expect_equal(trimws(out), "")
})

test_that("apa.knit.table.for.papaja errors without papaja installed", {
     skip_if(requireNamespace("papaja", quietly = TRUE),
             "papaja is installed, cannot test missing package error")
     t1 <- apa.cor.table(attitude)
     expect_error(apa.knit.table.for.papaja(t1), "papaja")
})
