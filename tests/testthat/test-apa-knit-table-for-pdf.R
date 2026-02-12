library(apaTables)

test_that("apa.knit.table.for.pdf works with correlation table", {
     t1 <- apa.cor.table(attitude)
     out <- apa.knit.table.for.pdf(t1)
     expect_true(nchar(out) > 0)
})

test_that("apa.knit.table.for.pdf works with regression table", {
     blk1 <- lm(sales ~ adverts + airplay + attract, data = album)
     t1 <- apa.reg.table(blk1)
     out <- apa.knit.table.for.pdf(t1)
     expect_true(nchar(out) > 0)
})

test_that("apa.knit.table.for.pdf works with aov table", {
     options(contrasts = c("contr.helmert", "contr.poly"))
     lm1 <- lm(libido ~ dose, data = viagra)
     t1 <- apa.aov.table(lm1)
     out <- apa.knit.table.for.pdf(t1)
     expect_true(nchar(out) > 0)
})

test_that("apa.knit.table.for.pdf works with oneway table", {
     t1 <- apa.1way.table(iv = dose, dv = libido, data = viagra)
     out <- apa.knit.table.for.pdf(t1)
     expect_true(nchar(out) > 0)
})

test_that("apa.knit.table.for.pdf works with twoway table", {
     t1 <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness, data = goggles)
     out <- apa.knit.table.for.pdf(t1)
     expect_true(nchar(out) > 0)
})

test_that("apa.knit.table.for.pdf works with twoway-ci table", {
     t1 <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                           data = goggles, show.conf.interval = TRUE)
     out <- apa.knit.table.for.pdf(t1)
     expect_true(nchar(out) > 0)
})

test_that("apa.knit.table.for.pdf works with dvalue table", {
     t1 <- apa.d.table(iv = dose, dv = libido, data = viagra)
     out <- apa.knit.table.for.pdf(t1)
     expect_true(nchar(out) > 0)
})

test_that("apa.knit.table.for.pdf custom table_title and table_note work", {
     t1 <- apa.cor.table(attitude)
     out <- apa.knit.table.for.pdf(t1, table_title = "Custom Title", table_note = "Custom Note")
     expect_true(nchar(out) > 0)
})
