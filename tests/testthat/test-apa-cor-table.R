library(apaTables)

test_that("apa.cor.table returns correct class and type with attitude data", {
     x <- apa.cor.table(attitude)
     expect_s3_class(x, "apa.table")
     expect_equal(x$table.type, "correlation")
})

test_that("apa.cor.table table.body has correct structure with attitude data", {
     x <- apa.cor.table(attitude)
     # attitude has 7 numeric columns -> 6 correlation columns
     expect_true(is.matrix(x$table.body))
     expect_equal(ncol(x$table.body), 10) # Variable, N, M, SD, 1-6
     expect_equal(colnames(x$table.body)[1], "Variable")
     expect_equal(colnames(x$table.body)[2], "N")
     expect_equal(colnames(x$table.body)[3], "M")
     expect_equal(colnames(x$table.body)[4], "SD")
     expect_equal(colnames(x$table.body)[5], "1")
     expect_equal(colnames(x$table.body)[10], "6")
})

test_that("apa.cor.table returns correct descriptive statistics", {
     x <- apa.cor.table(attitude)
     # rating: M = 64.63, SD = 12.17
     expect_equal(x$table.body[1,3], "64.63")
     expect_equal(x$table.body[1,4], "12.17")
     # first variable name
     expect_equal(x$table.body[1,1], "1. rating")
})

test_that("apa.cor.table returns correct correlation values", {
     x <- apa.cor.table(attitude)
     # complaints-rating r = .83**
     expect_equal(x$table.body[5,5], ".83**")
     # CI for that correlation
     expect_equal(x$table.body[6,5], "[.66, .91]")
})

test_that("apa.cor.table table.number parameter works", {
     x <- apa.cor.table(attitude, table.number = 5)
     expect_equal(x$table.number, 5)

     y <- apa.cor.table(attitude)
     expect_equal(y$table.number, 0)
})

test_that("apa.cor.table show.conf.interval=FALSE reduces row count", {
     x_with_ci <- apa.cor.table(attitude, show.conf.interval = TRUE, show.pvalue = FALSE)
     x_no_ci <- suppressMessages(apa.cor.table(attitude, show.conf.interval = FALSE, show.pvalue = FALSE))
     # With CI, each variable gets r + CI + blank = 3 rows
     # Without CI, each variable gets r + blank = 2 rows
     expect_true(nrow(x_with_ci$table.body) > nrow(x_no_ci$table.body))
})

test_that("apa.cor.table has RTF and LaTeX output components", {
     x <- apa.cor.table(attitude)
     expect_false(is.null(x$rtf.body))
     expect_false(is.null(x$latex.body))
     expect_false(is.null(x$rtf.table.title))
     expect_false(is.null(x$rtf.table.note))
     expect_false(is.null(x$latex.table.title))
     expect_false(is.null(x$latex.table.note))
})

test_that("apa.cor.table works with album dataset", {
     x <- apa.cor.table(album)
     expect_s3_class(x, "apa.table")
     # album has 4 numeric columns -> 3 correlation columns
     expect_equal(ncol(x$table.body), 7) # Variable, N, M, SD, 1-3
})
