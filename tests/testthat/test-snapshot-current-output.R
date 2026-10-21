# tests/testthat/test-snapshot-current-output.R
# Golden record of the builders' console output. These guard against
# regressions in what each builder prints. Do NOT accept snapshot changes
# without confirming the underlying statistics are unchanged.

test_that("apa.cor.table console output is stable", {
  expect_snapshot(print(apa.cor.table(attitude)))
})

test_that("apa.reg.table console output is stable", {
  blk1 <- lm(sales ~ adverts + airplay, data = album)
  blk2 <- lm(sales ~ adverts + airplay + attract, data = album)
  expect_snapshot(print(apa.reg.table(blk1, blk2, table.number = 2)))
})

test_that("apa.aov.table console output is stable", {
  old_contrasts <- options(contrasts = c("contr.treatment", "contr.poly"))
  on.exit(options(old_contrasts), add = TRUE)
  lm_output <- lm(libido ~ dose, data = viagra)
  expect_snapshot(print(apa.aov.table(lm_output, table.number = 4)))
})

test_that("apa.1way.table console output is stable", {
  expect_snapshot(print(apa.1way.table(iv = dose, dv = libido,
                                       data = viagra, table.number = 1)))
})

test_that("apa.2way.table console output is stable", {
  expect_snapshot(print(apa.2way.table(iv1 = gender, iv2 = alcohol,
                                       dv = attractiveness, data = goggles,
                                       table.number = 3)))
})

test_that("apa.d.table console output is stable", {
  expect_snapshot(print(apa.d.table(iv = dose, dv = libido, data = viagra)))
})
