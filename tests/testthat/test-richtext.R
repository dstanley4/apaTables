test_that("richtext wraps a plain string and flattens back", {
  rt <- richtext("Predictor")
  expect_s3_class(rt, "apa_richtext")
  expect_equal(as_plaintext(rt), "Predictor")
})

test_that("as_plaintext concatenates run text", {
  rt <- richtext(list(rt_run("b"), rt_run("2", script = "super")))
  expect_equal(as_plaintext(rt), "b2")
})

test_that("as_plaintext honours an explicit plain attribute", {
  rt <- richtext(list(rt_run("\u03b7"), rt_run("2", script = "super"),
                      rt_run("p", script = "sub")))
  attr(rt, "plain") <- "eta2p"
  expect_equal(as_plaintext(rt), "eta2p")
})

test_that("as_plaintext passes a bare string through", {
  expect_equal(as_plaintext("hello"), "hello")
})

test_that("richtext rejects list elements that are not runs", {
  expect_error(richtext(list(1, 2)), "run")
  expect_error(richtext(list(list(italic = TRUE))), "run")  # missing text
})

test_that("richtext accepts an empty list", {
  expect_silent(richtext(list()))
})

test_that("apa_sym builds italic stat symbols with plain fallbacks", {
  M <- apa_sym("M")
  expect_s3_class(M, "apa_richtext")
  expect_true(M[[1]]$italic)
  expect_equal(as_plaintext(M), "M")
})

test_that("apa_sym('eta2p') has eta + superscript 2 + subscript p", {
  e <- apa_sym("eta2p")
  scripts <- vapply(e, function(r) r$script, character(1))
  expect_true("super" %in% scripts && "sub" %in% scripts)
  expect_equal(as_plaintext(e), "eta2p")
})

test_that("apa_sym rejects unknown names", {
  expect_error(apa_sym("nope"), "unknown")
})

test_that("apa_stars renders superscript asterisks", {
  s <- apa_stars(2)
  expect_equal(s[[1]]$text, "**")
  expect_equal(s[[1]]$script, "super")
  expect_equal(as_plaintext(apa_stars(0)), "")
})

test_that("apa_ci uses txt.ci.brackets formatting", {
  expect_equal(as_plaintext(apa_ci(0.2, 0.55)), txt.ci.brackets(0.2, 0.55))
})

test_that("apa_ci_label builds an N% CI label", {
  expect_equal(as_plaintext(apa_ci_label(.95)), "95% CI")
})

test_that("richtext_lines stacks lines and flattens with newlines", {
  cell <- richtext_lines(".42**", apa_ci(0.20, 0.55))
  expect_s3_class(cell, "apa_richtext_lines")
  expect_equal(as_plaintext(cell),
               paste(".42**", txt.ci.brackets(0.20, 0.55), sep = "\n"))
})

test_that("apa_sym covers the ANOVA and d-value symbols", {
  expect_equal(as_plaintext(apa_sym("d")), "d")
  expect_true(apa_sym("d")[[1]]$italic)

  expect_equal(as_plaintext(apa_sym("MSE")), "MSE")
  expect_true(apa_sym("MSE")[[1]]$italic)

  # generalized eta-squared: eta, superscript 2, subscript g
  g <- apa_sym("eta2g")
  expect_equal(attr(g, "plain"), "eta2g")
  expect_equal(g[[1]]$text, "\u03b7"); expect_true(g[[1]]$italic)
  expect_equal(g[[2]]$text, "2");      expect_equal(g[[2]]$script, "super")
  expect_equal(g[[3]]$text, "g");      expect_equal(g[[3]]$script, "sub")

  # Greenhouse-Geisser epsilon (italic Greek epsilon)
  eps <- apa_sym("epsilon")
  expect_equal(eps[[1]]$text, "\u03b5"); expect_true(eps[[1]]$italic)
  expect_equal(attr(eps, "plain"), "epsilon")

  # df numerator / denominator: italic df + subscript
  dn <- apa_sym("df_num")
  expect_equal(dn[[1]]$text, "df"); expect_true(dn[[1]]$italic)
  expect_equal(dn[[2]]$text, "Num"); expect_equal(dn[[2]]$script, "sub")
  expect_equal(attr(dn, "plain"), "df_num")
  dd <- apa_sym("df_den")
  expect_equal(dd[[2]]$text, "Den"); expect_equal(dd[[2]]$script, "sub")
})
