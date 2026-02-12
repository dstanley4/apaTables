library(apaTables)

test_that("apa.d.table returns correct class and type", {
     t1 <- apa.d.table(iv = dose, dv = libido, data = viagra, table.number = 1)
     expect_s3_class(t1, "apa_table")
     expect_equal(t1$table.type, "dvalue")
})

test_that("apa.d.table table.body is a matrix with correct columns", {
     t1 <- apa.d.table(iv = dose, dv = libido, data = viagra)
     expect_true(is.matrix(t1$table_body))
     col_names <- colnames(t1$table_body)
     expect_true("Variable" %in% col_names)
     expect_true("M" %in% col_names)
     expect_true("SD" %in% col_names)
})

test_that("apa.d.table has correct dimensions", {
     t1 <- apa.d.table(iv = dose, dv = libido, data = viagra)
     # 3 dose levels -> 2 numbered d-value columns
     col_names <- colnames(t1$table_body)
     expect_true("1" %in% col_names)
     expect_true("2" %in% col_names)
     expect_equal(ncol(t1$table_body), 5) # Variable, M, SD, 1, 2
})

test_that("apa.d.table returns correct M and SD for first group", {
     t1 <- apa.d.table(iv = dose, dv = libido, data = viagra)
     # First group (Placebo): M = 2.20, SD = 1.30
     expect_equal(t1$table_body[1,2], "2.20")
     expect_equal(t1$table_body[1,3], "1.30")
})

test_that("apa.d.table first variable name starts with 1.", {
     t1 <- apa.d.table(iv = dose, dv = libido, data = viagra)
     expect_true(grepl("^1\\.", t1$table_body[1,1]))
})

test_that("apa.d.table table.number parameter works", {
     t1 <- apa.d.table(iv = dose, dv = libido, data = viagra, table.number = 6)
     expect_equal(t1$table_number, 6)
})

test_that("apa.d.table has RTF and LaTeX components", {
     t1 <- apa.d.table(iv = dose, dv = libido, data = viagra)
     expect_false(is.null(t1$rtf.body))
     expect_false(is.null(t1$rtf.table.title))
     expect_false(is.null(t1$latex.body))
     expect_false(is.null(t1$latex.table.title))
     expect_false(is.null(t1$latex.table.note))
})

test_that("apa.d.table has landscape set", {
     t1 <- apa.d.table(iv = dose, dv = libido, data = viagra)
     expect_true(t1$landscape)
})
