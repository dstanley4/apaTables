library(apaTables)

test_that("apa.2way.table returns correct class and type", {
     t1 <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                           data = goggles, table.number = 3)
     expect_s3_class(t1, "apa.table")
     expect_equal(t1$table.type, "twoway")
})

test_that("apa.2way.table title mentions DV and both IVs", {
     t1 <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                           data = goggles)
     expect_true(grepl("Attractiveness", t1$table.title))
     expect_true(grepl("Gender", t1$table.title))
     expect_true(grepl("Alcohol", t1$table.title))
})

test_that("apa.2way.table with CI changes table.type", {
     t1_no_ci <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                                 data = goggles)
     t1_ci <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                              data = goggles, show.conf.interval = TRUE)
     expect_equal(t1_no_ci$table.type, "twoway")
     expect_equal(t1_ci$table.type, "twoway-ci")
})

test_that("apa.2way.table show.marginal.means works without error", {
     expect_no_error(
          apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                         data = goggles, show.marginal.means = TRUE)
     )
})

test_that("apa.2way.table table.number parameter works", {
     t1 <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                           data = goggles, table.number = 8)
     expect_equal(t1$table.number, 8)
})

test_that("apa.2way.table works with fidler_thompson data", {
     t1 <- apa.2way.table(iv1 = a, iv2 = b, dv = dv,
                           data = fidler_thompson, table.number = 2)
     expect_s3_class(t1, "apa.table")
     expect_true(grepl("Dv", t1$table.title))
     expect_true(grepl("A", t1$table.title))
     expect_true(grepl("B", t1$table.title))
})

test_that("apa.2way.table has RTF and LaTeX components", {
     t1 <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                           data = goggles)
     expect_false(is.null(t1$rtf.body))
     expect_false(is.null(t1$rtf.table.title))
     expect_false(is.null(t1$latex.body))
     expect_false(is.null(t1$latex.table.title))
     expect_false(is.null(t1$latex.table.note))
})

test_that("apa.2way.table table.body is not null", {
     t1 <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                           data = goggles)
     expect_false(is.null(t1$table.body))
     expect_true(nrow(t1$table.body) > 0)
     expect_true(ncol(t1$table.body) > 0)
})
