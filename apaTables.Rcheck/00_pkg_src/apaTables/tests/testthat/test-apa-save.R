library(apaTables)

test_that("apa.save creates a file on disk", {
     t1 <- apa.1way.table(iv = dose, dv = libido, data = viagra, table.number = 1)
     tmp <- tempfile(fileext = ".doc")
     on.exit(unlink(tmp))
     apa.save(filename = tmp, t1)
     expect_true(file.exists(tmp))
})

test_that("apa.save creates non-empty file", {
     t1 <- apa.1way.table(iv = dose, dv = libido, data = viagra, table.number = 1)
     tmp <- tempfile(fileext = ".doc")
     on.exit(unlink(tmp))
     apa.save(filename = tmp, t1)
     expect_true(file.info(tmp)$size > 0)
})

test_that("apa.save creates file with RTF content", {
     t1 <- apa.1way.table(iv = dose, dv = libido, data = viagra, table.number = 1)
     tmp <- tempfile(fileext = ".doc")
     on.exit(unlink(tmp))
     apa.save(filename = tmp, t1)
     content <- readLines(tmp, n = 1)
     expect_true(grepl("rtf", content))
})

test_that("apa.save works with multiple tables", {
     t1 <- apa.1way.table(iv = dose, dv = libido, data = viagra, table.number = 1)
     t2 <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                           data = goggles, table.number = 2)
     tmp <- tempfile(fileext = ".doc")
     on.exit(unlink(tmp))
     apa.save(filename = tmp, t1, t2)
     expect_true(file.exists(tmp))
     expect_true(file.info(tmp)$size > 0)
})

test_that("apa.save paper='a4' parameter works", {
     t1 <- apa.1way.table(iv = dose, dv = libido, data = viagra, table.number = 1)
     tmp <- tempfile(fileext = ".doc")
     on.exit(unlink(tmp))
     apa.save(filename = tmp, t1, paper = "a4")
     expect_true(file.exists(tmp))
     expect_true(file.info(tmp)$size > 0)
})

test_that("apa.save works with regression tables", {
     blk1 <- lm(sales ~ adverts + airplay + attract, data = album)
     r1 <- apa.reg.table(blk1, table.number = 1)
     tmp <- tempfile(fileext = ".doc")
     on.exit(unlink(tmp))
     apa.save(filename = tmp, r1)
     expect_true(file.exists(tmp))
     expect_true(file.info(tmp)$size > 0)
})

test_that("apa.save works with aov tables", {
     options(contrasts = c("contr.helmert", "contr.poly"))
     lm1 <- lm(libido ~ dose, data = viagra)
     t1 <- apa.aov.table(lm1, table.number = 1)
     tmp <- tempfile(fileext = ".doc")
     on.exit(unlink(tmp))
     apa.save(filename = tmp, t1)
     expect_true(file.exists(tmp))
})

test_that("apa.save works with correlation tables", {
     t1 <- apa.cor.table(attitude, table.number = 1)
     tmp <- tempfile(fileext = ".doc")
     on.exit(unlink(tmp))
     apa.save(filename = tmp, t1)
     expect_true(file.exists(tmp))
     expect_true(file.info(tmp)$size > 0)
})

test_that("apa.save works with d-value tables", {
     t1 <- apa.d.table(iv = dose, dv = libido, data = viagra, table.number = 1)
     tmp <- tempfile(fileext = ".doc")
     on.exit(unlink(tmp))
     apa.save(filename = tmp, t1)
     expect_true(file.exists(tmp))
})
