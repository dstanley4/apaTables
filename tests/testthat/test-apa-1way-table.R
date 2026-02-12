library(apaTables)

test_that("apa.1way.table returns correct class and type", {
     t1 <- apa.1way.table(iv = dose, dv = libido, data = viagra, table.number = 1)
     expect_s3_class(t1, "apa.table")
     expect_equal(t1$table.type, "oneway")
})

test_that("apa.1way.table has correct table.body structure", {
     t1 <- apa.1way.table(iv = dose, dv = libido, data = viagra)
     expect_true(is.data.frame(t1$table.body))
     expect_equal(nrow(t1$table.body), 3) # Placebo, Low Dose, High Dose
     expect_true("M" %in% names(t1$table.body))
     expect_true("SD" %in% names(t1$table.body))
})

test_that("apa.1way.table returns correct descriptive values", {
     t1 <- apa.1way.table(iv = dose, dv = libido, data = viagra)
     # Placebo group: M = 2.20, SD = 1.30
     expect_equal(t1$table.body$M[1], "2.20")
     expect_equal(t1$table.body$SD[1], "1.30")
     # High Dose group: M = 5.00
     expect_equal(t1$table.body$M[3], "5.00")
})

test_that("apa.1way.table show.conf.interval adds CI column", {
     t1_no_ci <- apa.1way.table(iv = dose, dv = libido, data = viagra)
     t1_ci <- apa.1way.table(iv = dose, dv = libido, data = viagra, show.conf.interval = TRUE)
     expect_false("M_95%_CI" %in% names(t1_no_ci$table.body))
     expect_true("M_95%_CI" %in% names(t1_ci$table.body))
     expect_equal(ncol(t1_ci$table.body), ncol(t1_no_ci$table.body) + 1)
})

test_that("apa.1way.table table.number parameter works", {
     t1 <- apa.1way.table(iv = dose, dv = libido, data = viagra, table.number = 7)
     expect_equal(t1$table.number, 7)
})

test_that("apa.1way.table title mentions DV and IV names", {
     t1 <- apa.1way.table(iv = dose, dv = libido, data = viagra)
     expect_true(grepl("Libido", t1$table.title))
     expect_true(grepl("Dose", t1$table.title))
})

test_that("apa.1way.table has RTF and LaTeX components", {
     t1 <- apa.1way.table(iv = dose, dv = libido, data = viagra)
     expect_false(is.null(t1$rtf.body))
     expect_false(is.null(t1$rtf.table.title))
     expect_false(is.null(t1$latex.table.title))
     expect_false(is.null(t1$latex.table.note))
})
