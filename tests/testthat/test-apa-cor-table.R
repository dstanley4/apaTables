library(apaTables)

test_that("apa.cor.table returns correct class and type with attitude data", {
     x <- apa.cor.table(attitude)
     expect_s3_class(x, "apa_table")
     expect_equal(x$table.type, "correlation")
})

test_that("apa.cor.table table.body has correct structure with attitude data", {
     x <- apa.cor.table(attitude)
     # attitude has 7 numeric columns -> 6 correlation columns
     expect_true(is.matrix(x$table_body))
     expect_equal(ncol(x$table_body), 10) # Variable, N, M, SD, 1-6
     expect_equal(colnames(x$table_body)[1], "Variable")
     expect_equal(colnames(x$table_body)[2], "N")
     expect_equal(colnames(x$table_body)[3], "M")
     expect_equal(colnames(x$table_body)[4], "SD")
     expect_equal(colnames(x$table_body)[5], "1")
     expect_equal(colnames(x$table_body)[10], "6")
})

test_that("apa.cor.table returns correct descriptive statistics", {
     x <- apa.cor.table(attitude)
     # rating: M = 64.63, SD = 12.17
     expect_equal(x$table_body[1,3], "64.63")
     expect_equal(x$table_body[1,4], "12.17")
     # first variable name
     expect_equal(x$table_body[1,1], "1. rating")
})

test_that("apa.cor.table returns correct correlation values", {
     x <- apa.cor.table(attitude)
     # complaints-rating r = .83**
     expect_equal(x$table_body[5,5], ".83**")
     # CI for that correlation
     expect_equal(x$table_body[6,5], "[.66, .91]")
})

test_that("apa.cor.table table.number parameter works", {
     x <- apa.cor.table(attitude, table.number = 5)
     expect_equal(x$table_number, 5)

     y <- apa.cor.table(attitude)
     expect_equal(y$table_number, 1L)
})

test_that("apa.cor.table show.conf.interval=FALSE is deprecated and ignored", {
     # FALSE now emits a deprecation message and is otherwise ignored
     expect_message(apa.cor.table(attitude, show.conf.interval = FALSE), "deprecated")
     x_with_ci <- apa.cor.table(attitude, show.pvalue = FALSE)
     x_no_ci <- suppressMessages(apa.cor.table(attitude, show.conf.interval = FALSE, show.pvalue = FALSE))
     # CIs are forced on, so FALSE yields the same row count as the default (TRUE)
     expect_equal(nrow(x_with_ci$table_body), nrow(x_no_ci$table_body))
})

test_that("apa.cor.table has LaTeX output components", {
     x <- apa.cor.table(attitude)
     expect_false(is.null(x$latex.body))
     expect_false(is.null(x$latex.table.title))
     expect_false(is.null(x$latex.table.note))
})

test_that("apa.cor.table works with album dataset", {
     x <- apa.cor.table(album)
     expect_s3_class(x, "apa_table")
     # album has 4 numeric columns -> 3 correlation columns
     expect_equal(ncol(x$table_body), 7) # Variable, N, M, SD, 1-3
})

test_that("a call written for 2.0.8, with landscape passed by position, keeps its meaning", {
  # 2.0.8's order was data, filename, table.number, show.conf.interval,
  # show.sig.stars, landscape. show.pvalue is new and comes after landscape,
  # so the sixth positional value is still landscape.
  x <- apa.cor.table(attitude[, 1:3], NA, 1, TRUE, TRUE, FALSE)
  expect_false(x$landscape)
  expect_true(any(grepl("^p ", unlist(x$table_body))))
  expect_identical(names(formals(apa.cor.table))[1:6],
                   c("data", "filename", "table.number", "show.conf.interval",
                     "show.sig.stars", "landscape"))
})
