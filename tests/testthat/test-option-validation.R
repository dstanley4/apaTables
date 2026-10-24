# Out-of-range option values stop with a message naming the allowed values;
# none is silently replaced by a default.

test_that("landscape must be TRUE, FALSE or NA in every table function", {
  msg <- "landscape must be TRUE, FALSE or NA"
  bad <- list("yes", 1, c(TRUE, FALSE), NULL)
  for (value in bad) {
    expect_error(apa.cor.table(data = attitude[, 1:3], landscape = value), msg)
    expect_error(apa.d.table(iv = dose, dv = libido, data = viagra, landscape = value), msg)
    expect_error(apa.1way.table(iv = dose, dv = libido, data = viagra, landscape = value), msg)
    expect_error(apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                                data = goggles, landscape = value), msg)
    expect_error(apa.reg.table(lm(rating ~ complaints, data = attitude), landscape = value), msg)
    expect_error(apa.aov.table(lm(libido ~ dose, data = viagra), landscape = value), msg)
  }
  # the three documented values are accepted
  for (value in list(TRUE, FALSE, NA)) {
    expect_s3_class(apa.cor.table(data = attitude[, 1:3], landscape = value), "apa_table")
  }
})

test_that("apa.afex.table() checks landscape before anything else", {
  expect_error(apa.afex.table(afex.output = NULL, landscape = "yes"),
               "landscape must be TRUE, FALSE or NA")
})

test_that("table.number must be a whole number of 0 or more", {
  msg <- "table.number must be a whole number"
  expect_error(apa.cor.table(data = attitude[, 1:3], table.number = -1), msg)
  expect_error(apa.cor.table(data = attitude[, 1:3], table.number = 2.5), msg)
  expect_identical(apa.cor.table(data = attitude[, 1:3], table.number = 2)$table_number, 2L)
  expect_true(is.na(apa.cor.table(data = attitude[, 1:3], table.number = 0)$table_number))
})

test_that("confidence levels must be .90 or .95", {
  m <- lm(rating ~ complaints + privileges, data = attitude)
  expect_error(apa.reg.table(m, prop.var.conf.level = .99), "prop.var.conf.level must be .90 or .95")
  expect_error(apa.reg.table(m, prop.var.conf.level = 90), "prop.var.conf.level must be .90 or .95")
  expect_error(apa.aov.table(lm(libido ~ dose, data = viagra), conf.level = .99),
               "conf.level must be .90 or .95")
  # a value computed to .90 is still .90
  expect_s3_class(apa.reg.table(m, prop.var.conf.level = 1 - .1), "apa_table")
})

test_that("table.number = Inf gets the named message", {
  expect_error(apa.cor.table(data = attitude[, 1:3], table.number = Inf),
               "table.number must be a whole number")
})
