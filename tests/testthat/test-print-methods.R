library(apaTables)

test_that("print.apa.table outputs Table N and title", {
     t1 <- apa.1way.table(iv = dose, dv = libido, data = viagra, table.number = 3)
     out <- capture.output(print(t1))
     expect_true(any(grepl("Table 3", out)))
     expect_true(any(grepl("Descriptive Statistics", out)))
})

test_that("print.apa.table outputs table note", {
     t1 <- apa.1way.table(iv = dose, dv = libido, data = viagra, table.number = 3)
     out <- capture.output(print(t1))
     expect_true(any(grepl("Note", out)))
})

test_that("print.apa_table outputs Table N and title", {
     options(contrasts = c("contr.helmert", "contr.poly"))
     lm1 <- lm(libido ~ dose, data = viagra)
     t1 <- apa.aov.table(lm1, table.number = 5)
     out <- capture.output(print(t1))
     expect_true(any(grepl("Table 5", out)))
     expect_true(any(grepl("ANOVA", out)))
})

test_that("print.apa_table for regression outputs Regression", {
     blk1 <- lm(sales ~ adverts + airplay + attract, data = album)
     r1 <- apa.reg.table(blk1, table.number = 2)
     out <- capture.output(print(r1))
     expect_true(any(grepl("Table 2", out)))
     expect_true(any(grepl("Regression", out)))
})

test_that("print.apa.table with table.number = 0 does not error", {
     t1 <- apa.1way.table(iv = dose, dv = libido, data = viagra, table.number = 0)
     out <- capture.output(print(t1))
     # Should still produce output without error
     expect_true(length(out) > 0)
})

test_that("print.apa_table with table.number = 0 does not error", {
     options(contrasts = c("contr.helmert", "contr.poly"))
     lm1 <- lm(libido ~ dose, data = viagra)
     t1 <- apa.aov.table(lm1, table.number = 0)
     out <- capture.output(print(t1))
     expect_true(length(out) > 0)
})
