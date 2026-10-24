# Ordinary input mistakes stop with a message that says what to change,
# rather than a base-R error from inside the package; and a grouping factor
# with unused levels (common after subset()) builds a table.

test_that("the means tables need a numeric dv", {
  expect_error(apa.1way.table(iv = dose, dv = dose, data = viagra),
               "apa.1way.table() needs a numeric dv, but dose is a factor.", fixed = TRUE)
  expect_error(apa.2way.table(iv1 = gender, iv2 = alcohol, dv = gender, data = goggles),
               "apa.2way.table() needs a numeric dv, but gender is a factor.", fixed = TRUE)
  expect_error(apa.d.table(iv = dose, dv = dose, data = viagra),
               "apa.d.table() needs a numeric dv, but dose is a factor.", fixed = TRUE)
  txt <- data.frame(g = factor(c("a", "a", "b", "b")), y = c("1", "2", "3", "4"))
  expect_error(apa.1way.table(iv = g, dv = y, data = txt), "but y is text.", fixed = TRUE)
})

test_that("unused factor levels are left out of the means tables", {
  unused <- data.frame(g = factor(c("a", "a", "a", "b", "b", "b"), levels = c("a", "b", "c")),
                       y = c(1, 2, 4, 3, 5, 9))
  t1 <- apa.1way.table(iv = g, dv = y, data = unused, show.conf.interval = TRUE)
  expect_equal(as.character(t1$table_body$g), c("a", "b"))
  td <- apa.d.table(iv = g, dv = y, data = unused)
  expect_equal(unname(trimws(td$table_body[, 1]))[c(1, 3)], c("1. a", "2. b"))
  expect_false(any(grepl("^3\\. ", trimws(td$table_body[, 1]))))
  g2 <- goggles
  g2$alcohol <- factor(g2$alcohol, levels = c(levels(g2$alcohol), "6 Pints"))
  t2 <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness, data = g2,
                       show.conf.interval = TRUE)
  expect_false(any(grepl("6 Pints", unlist(t2$table_body), fixed = TRUE)))
})

test_that("a TRUE/FALSE dv gives proportions, as in 2.0.8", {
  v <- viagra
  v$high <- v$libido > 3
  t1 <- apa.1way.table(iv = dose, dv = high, data = v)
  expect_equal(unname(t1$table_body$M), c("0.20", "0.40", "0.80"))
  g <- goggles
  g$high <- g$attractiveness > 55
  expect_s3_class(apa.2way.table(iv1 = gender, iv2 = alcohol, dv = high, data = g), "apa_table")
  expect_s3_class(apa.d.table(iv = dose, dv = high, data = v), "apa_table")
})

test_that("a dv with no values, or a group whose cases have none, is named", {
  expect_error(apa.1way.table(iv = g, dv = y, data = data.frame(g = factor(c("a", "a", "b")), y = c(NA_real_, NA, NA))),
               "apa.1way.table() found no values in y.", fixed = TRUE)
  expect_error(apa.2way.table(iv1 = gender, iv2 = alcohol, dv = y, data = transform(goggles, y = NA_real_)),
               "apa.2way.table() found no values in y.", fixed = TRUE)
  part <- data.frame(g = factor(c("a", "a", "b", "b", "c", "c")), y = c(1, 2, NA, NA, 4, 6))
  expect_error(apa.1way.table(iv = g, dv = y, data = part), "Too few in: g = b (0 cases).", fixed = TRUE)
  expect_error(apa.d.table(iv = g, dv = y, data = part), "Too few in: g = b (0 cases).", fixed = TRUE)
})

test_that("a group or cell with fewer than two cases is named", {
  one <- data.frame(g = factor(c("a", "a", "b")), y = c(1, 2, 3))
  msg <- "needs at least two cases with a value on y in each group, to compute a standard deviation. Too few in: g = b (1 case)."
  expect_error(apa.1way.table(iv = g, dv = y, data = one), msg, fixed = TRUE)
  expect_error(apa.1way.table(iv = g, dv = y, data = one, show.conf.interval = TRUE), msg, fixed = TRUE)
  expect_error(apa.d.table(iv = g, dv = y, data = one), msg, fixed = TRUE)
  g3 <- goggles[-(2:8), ]
  expect_error(apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness, data = g3),
               "values on gender, alcohol and attractiveness in each cell, to compute a standard deviation. Too few in: gender = Female, alcohol = None (1 case).",
               fixed = TRUE)
  # an empty cell whose levels are used elsewhere is a cell with no cases
  g4 <- goggles[!(goggles$gender == "Male" & goggles$alcohol == "None"), ]
  expect_error(apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness, data = g4),
               "gender = Male, alcohol = None (0 cases)", fixed = TRUE)
})

test_that("apa.d.table() needs two groups", {
  expect_error(apa.d.table(iv = g, dv = y, data = data.frame(g = factor(rep("a", 6)), y = 1:6)),
               "apa.d.table() needs at least two groups in g with values on y, but it has only one (a).",
               fixed = TRUE)
})

test_that("apa.cor.table() needs two numeric columns with variation", {
  expect_error(apa.cor.table(data = data.frame(a = 1:5, b = letters[1:5])),
               "apa.cor.table() needs at least two numeric columns in data, but data has 1. Columns that are not numbers", fixed = TRUE)
  expect_error(apa.cor.table(data = data.frame()),
               "but data has 0.", fixed = TRUE)
  expect_error(apa.cor.table(data = data.frame(a = c(1, 2, 3, 4), b = c(2, 2, 2, 2), c = c(4, 1, 3, 2))),
               "apa.cor.table() cannot correlate a column with fewer than two different values: b.",
               fixed = TRUE)
  # non-numeric columns are still left out quietly, as documented
  vars <- unname(trimws(apa.cor.table(data = iris)$table_body[, 1]))
  expect_equal(vars[nzchar(vars)], c("1. Sepal.Length", "2. Sepal.Width", "3. Petal.Length", "4. Petal.Width"))
})

test_that("apa.reg.table() and apa.aov.table() need lm() models", {
  expect_error(apa.reg.table(), "apa.reg.table() needs at least one model fitted with lm().", fixed = TRUE)
  expect_error(apa.reg.table(glm(am ~ wt, data = mtcars, family = binomial)),
               "apa.reg.table() needs models fitted with lm(), but the model was fitted with glm().",
               fixed = TRUE)
  expect_error(apa.reg.table(data.frame(a = 1)), "but the model is a data frame.", fixed = TRUE)
  m1 <- lm(mpg ~ wt, data = mtcars)
  expect_error(apa.reg.table(m1, glm(mpg ~ wt + hp, data = mtcars)),
               "but block 2 was fitted with glm().", fixed = TRUE)
  expect_error(apa.reg.table(lm(mpg ~ 1, data = mtcars)),
               "apa.reg.table() needs at least one predictor in each model, but the model has none.",
               fixed = TRUE)
  expect_error(apa.reg.table(lm(mpg ~ 0 + wt, data = mtcars)),
               "apa.reg.table() needs models with an intercept, but the model was fitted without one",
               fixed = TRUE)
  expect_error(apa.reg.table(aov(mpg ~ factor(cyl), data = mtcars)),
               "but the model was fitted with aov(); fit it with lm() instead.", fixed = TRUE)
  expect_error(apa.aov.table(aov(attractiveness ~ gender + Error(factor(seq_len(nrow(goggles)))), data = goggles)),
               "Error() term (for repeated measures, use apa.afex.table())", fixed = TRUE)
  expect_error(apa.reg.table(lm(cbind(mpg, hp) ~ wt, data = mtcars)),
               "but the model has more than one (cbind() on the left of ~).", fixed = TRUE)
  expect_error(apa.aov.table(1:3), "apa.aov.table() needs models fitted with lm(), but the model is numbers.", fixed = TRUE)
  expect_error(apa.aov.table(glm(am ~ factor(cyl), data = mtcars, family = binomial)),
               "but the model was fitted with glm().", fixed = TRUE)
  # an aov() fit is an lm fit, and still accepted
  expect_s3_class(apa.aov.table(aov(attractiveness ~ gender * alcohol, data = goggles)), "apa_table")
})

test_that("show/hide arguments must be TRUE or FALSE", {
  expect_error(apa.cor.table(data = attitude[, 1:3], show.pvalue = NA),
               "show.pvalue must be TRUE or FALSE.", fixed = TRUE)
  expect_error(apa.cor.table(data = attitude[, 1:3], show.sig.stars = "yes"),
               "show.sig.stars must be TRUE or FALSE.", fixed = TRUE)
  expect_error(apa.1way.table(iv = dose, dv = libido, data = viagra, show.conf.interval = NA),
               "show.conf.interval must be TRUE or FALSE.", fixed = TRUE)
  expect_error(apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness, data = goggles,
                              show.marginal.means = c(TRUE, FALSE)),
               "show.marginal.means must be TRUE or FALSE.", fixed = TRUE)
  expect_error(apa.d.table(iv = dose, dv = libido, data = viagra, show.conf.interval = NA),
               "show.conf.interval must be TRUE or FALSE.", fixed = TRUE)
  expect_error(apa.cor.table(data = attitude[, 1:3], show.conf.interval = NA),
               "show.conf.interval must be TRUE or FALSE.", fixed = TRUE)
  expect_error(apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness, data = goggles,
                              show.conf.interval = NA),
               "show.conf.interval must be TRUE or FALSE.", fixed = TRUE)
  expect_error(apa.1way.table(iv = dose, dv = libido, data = viagra, show.conf.interval = 2),
               "show.conf.interval must be TRUE or FALSE.", fixed = TRUE)
})

test_that("1 and 0 still work for TRUE and FALSE, as in 2.0.8", {
  one <- apa.1way.table(iv = dose, dv = libido, data = viagra, show.conf.interval = 1)
  true <- apa.1way.table(iv = dose, dv = libido, data = viagra, show.conf.interval = TRUE)
  expect_identical(one$table_body, true$table_body)
  expect_identical(one$spec, true$spec)
  m1 <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness, data = goggles, show.marginal.means = 1)
  mt <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness, data = goggles, show.marginal.means = TRUE)
  expect_identical(m1$spec, mt$spec)
  expect_identical(apa.cor.table(data = attitude[, 1:3], show.pvalue = 0)$spec,
                   apa.cor.table(data = attitude[, 1:3], show.pvalue = FALSE)$spec)
})

test_that("filename takes one name, NA or NULL, in every builder", {
  builders <- list(
    cor  = function(f) apa.cor.table(data = attitude[, 1:3], filename = f),
    reg  = function(f) apa.reg.table(lm(mpg ~ wt, data = mtcars), filename = f),
    aov  = function(f) apa.aov.table(lm(attractiveness ~ gender, data = goggles), filename = f),
    oneway = function(f) apa.1way.table(iv = dose, dv = libido, data = viagra, filename = f),
    twoway = function(f) apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness, data = goggles, filename = f),
    d    = function(f) apa.d.table(iv = dose, dv = libido, data = viagra, filename = f))
  if (requireNamespace("afex", quietly = TRUE)) {
    fit <- suppressMessages(afex::aov_ez(id = "participant", dv = "attractiveness", data = goggles,
                                         between = c("gender", "alcohol")))
    builders$afex <- function(f) apa.afex.table(afex.output = fit, filename = f)
  }
  for (nm in names(builders)) {
    expect_s3_class(builders[[nm]](NULL), "apa_table")
    for (bad in list(c("a.docx", "b.docx"), 3, "")) {
      expect_error(builders[[nm]](bad), "filename must be one file name", fixed = TRUE, info = nm)
    }
  }
})

test_that("apa.save() names a missing file name and a non-table", {
  t1 <- apa.cor.table(data = attitude[, 1:3])
  expect_error(apa.save(t1), "apa.save() needs a file name and then the tables", fixed = TRUE)
  expect_error(apa.save(t1, mtcars, filename = file.path(tempdir(), "x.docx")),
               "apa.save() needs tables made by an apaTables function such as apa.cor.table() or apa.reg.table(), but one of the tables given is a data frame.",
               fixed = TRUE)
  expect_error(apa.save(filename = c("a.docx", "b.docx"), t1), "filename must be one file name", fixed = TRUE)
  expect_error(apa.save(filename = file.path(tempdir(), "x.docx")), "apa.save() needs at least one table", fixed = TRUE)
  old <- structure(list(), class = "apa.table")
  expect_error(apa.save(filename = file.path(tempdir(), "x.docx"), old),
               "a table made by an older version of apaTables", fixed = TRUE)
  # an exported helper apa.save() calls itself is not named as the culprit
  fake <- structure(list(), class = "apa_table")
  expect_error(apa.save(filename = file.path(tempdir(), "x.docx"), fake), "apa.save() needs a table", fixed = TRUE)
})

test_that("a non-table given to a Quarto entry point names the function the user called", {
  expect_error(apa.quarto.pdf(x = mtcars),
               "apa.quarto.pdf() needs a table made by an apaTables function such as apa.cor.table() or apa.reg.table(), but it was given a data frame.",
               fixed = TRUE)
  expect_error(apa.quarto.template(x = 1:3), "apa.quarto.template() needs a table", fixed = TRUE)
  expect_error(apa.caption(x = NULL), "apa.caption() needs a table", fixed = TRUE)
  expect_error(lapply(list(mtcars), apa.quarto.pdf), "apa.quarto.pdf() needs a table", fixed = TRUE)
  expect_error(do.call(apa.quarto.docx, list(x = "x")), "apa.quarto.docx() needs a table made by an apaTables function such as apa.cor.table() or apa.reg.table(), but it was given text.", fixed = TRUE)
})

test_that("printing a table returns it invisibly", {
  t1 <- apa.cor.table(data = attitude[, 1:3])
  vis <- NULL
  utils::capture.output(vis <- withVisible(print(t1)))
  expect_identical(vis$value, t1)
  expect_false(vis$visible)
})

test_that("small and two-valued groups give pooled d-values with no mark and no warning", {
  sets <- list(
    two_each   = data.frame(g = factor(c("a", "a", "b", "b")), y = c(1, 2, 3, 4)),
    three_grps = data.frame(g = factor(rep(c("a", "b", "c"), each = 2)), y = c(1, 3, 10, 20, 5, 6)),
    large      = data.frame(g = factor(c("a", "a", "b", "b")), y = 1e10 + c(0.49, 0.32, 0.56, 0.26)),
    tf_split   = data.frame(g = factor(rep(c("control", "treatment"), each = 10)),
                            y = c(rep(c(TRUE, FALSE), 5), rep(TRUE, 10))),
    two_valued = data.frame(g = factor(rep(c("a", "b"), each = 6)), y = c(1, 1, 1, 5, 5, 5, 3, 3, 3, 4, 4, 4)))
  for (nm in names(sets)) {
    expect_no_warning(t1 <- apa.d.table(iv = g, dv = y, data = sets[[nm]]))
    expect_length(t1$spec$notes, 1)
    expect_false(any(grepl("^[0-9.]+a$", trimws(unlist(t1$table_body)))), label = nm)
  }
  two <- sets$two_valued
  expect_equal(unname(apa.d.table(iv = g, dv = y, data = two)$table_body[3, "1"]),
               sprintf("%1.2f", get_d_value(two$y[two$g == "b"], two$y[two$g == "a"])))
})

test_that("a difftime dv is used as numbers, as in 2.0.8", {
  d <- data.frame(g = factor(rep(c("a", "b"), each = 5)), rt = as.difftime(c(1:5, 3:7), units = "secs"))
  dn <- transform(d, rt = as.numeric(rt))
  expect_identical(apa.1way.table(iv = g, dv = rt, data = d, show.conf.interval = TRUE)$table_body,
                   apa.1way.table(iv = g, dv = rt, data = dn, show.conf.interval = TRUE)$table_body)
})

test_that("a two-way cell message counts complete cases", {
  set.seed(2)
  d <- data.frame(g = factor(rep(c("a", "b", "c"), each = 6)), h = factor(rep(c("x", "y"), 9)), y = round(rnorm(18), 2))
  d$h[d$g == "c"] <- NA
  expect_error(apa.2way.table(iv1 = g, iv2 = h, dv = y, data = d),
               "needs at least two cases with values on g, h and y in each cell", fixed = TRUE)
})

test_that("a grouping variable with no values is named", {
  e <- data.frame(g = factor(c(NA, NA, NA), levels = c("a", "b")), y = c(1, 2, 3))
  expect_error(apa.1way.table(iv = g, dv = y, data = e), "apa.1way.table() found no values in g.", fixed = TRUE)
})

test_that("a model with two dependent variables is named as such", {
  expect_error(apa.aov.table(lm(cbind(mpg, hp) ~ factor(cyl), data = mtcars)),
               "apa.aov.table() needs a model with one dependent variable, but the model has more than one",
               fixed = TRUE)
})
