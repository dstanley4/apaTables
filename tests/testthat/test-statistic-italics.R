# APA sets a statistic's symbol in italics wherever it appears -- in a column
# heading, in the body of the table, and in the note. These tests exist because
# two places had drifted from that rule while every neighbouring symbol obeyed
# it, which is what made both easy to miss by reading:
#
#   * apa.cor.table() stacked "p < .001" into the body as ONE PLAIN RUN, so all
#     21 p cells of apa.cor.table(attitude) printed an upright p. It was a SPEC
#     defect, so Word, HTML and PDF were wrong identically and no emitter could
#     have corrected it -- once the italic flag is gone, nothing downstream can
#     tell a statistic from ordinary text.
#   * apa.afex.table()'s note said "p-values" in running prose with a roman p,
#     surrounded by a correctly italic df, epsilon, MSE and eta-squared.
#
# The sweep below is deliberately over the REAL builders rather than a synthetic
# spec: the flextable snapshot uses a hand-built two-row demo spec with no p
# cells, which is precisely why it stayed green through 21 wrong ones.

test_that("apa.cor.table stacks its p line with the statistic italic", {
  sp <- as_spec(apa.cor.table(attitude))
  seen <- 0L
  for (row in sp$body) for (cell in row) {
    if (!inherits(cell, "apa_richtext_lines")) next
    for (line in cell) {
      if (!grepl("^\\s*p\\s*[<=]", as_plaintext(line))) next
      seen <- seen + 1L
      expect_match(rt_to_latex(line), "\\\\textit\\{p\\}")
      expect_match(rt_to_markdown(line), "\\*p\\*")
    }
  }
  # the assertion above is worthless if the loop never runs
  expect_gt(seen, 0L)
})

test_that("a p line that is not a p line is left alone", {
  # the split is anchored, so anything unexpected passes through unchanged
  # rather than being silently reshaped
  expect_equal(rt_to_latex(cor_pvalue_richtext("n.s.")), "n.s.")
  expect_equal(rt_to_latex(cor_pvalue_richtext("")), "")
})

test_that("no generated note leaves a statistic symbol roman", {
  skip_if_not_installed("afex")
  op <- options(contrasts = c("contr.sum", "contr.poly"))
  on.exit(options(op), add = TRUE)

  specs <- list(
    cor = apa.cor.table(attitude),
    reg = apa.reg.table(stats::lm(sales ~ adverts, data = album)),
    d   = apa.d.table(iv = dose, dv = libido, data = viagra),
    aov = apa.aov.table(stats::lm(libido ~ dose, data = viagra)),
    one = apa.1way.table(iv = dose, dv = libido, data = viagra),
    two = apa.2way.table(iv1 = gender, iv2 = alcohol,
                         dv = attractiveness, data = goggles))

  # A bare statistic letter, i.e. one NOT already inside *...* emphasis and NOT
  # inside ~...~ or ^...^ script markup. The subscript of eta-squared-partial is
  # a descriptive label and is correctly roman, so script spans are excluded
  # rather than matched -- that exclusion is what keeps this from firing on
  # apa.aov.table's legitimate eta^2~p~.
  bare <- "(?<![*A-Za-z~^])(p|F|t|r|d|M|N|SD|df|MS|SS|MSE)(?![*A-Za-z~^])"

  for (nm in names(specs)) {
    for (note in as_spec(specs[[nm]])$notes) {
      md <- rt_to_markdown(note)
      # A BACKSLASH-ESCAPED asterisk is a literal character in the stars
      # sentence ("\\* indicates *p* < .05."), not emphasis. It must go first:
      # leaving it in makes the emphasis stripper below start its match on the
      # wrong asterisk, which strands the p of "*p*" and reports a false
      # positive on a note that is perfectly correct.
      stripped <- gsub("\\\\\\*", "", md)              # literal \* escapes
      stripped <- gsub("\\*[^*]+\\*", "", stripped)    # remove italic spans
      stripped <- gsub("[~^][^~^]+[~^]", "", stripped) # remove script spans
      hits <- regmatches(stripped, gregexpr(bare, stripped, perl = TRUE))[[1]]
      expect_identical(
        hits, character(0),
        info = paste0(nm, " note leaves a roman statistic symbol: ",
                      paste(unique(hits), collapse = " "), "\n  note: ", md))
    }
  }
})

test_that("the afex note italicises the p in p-values", {
  skip_if_not_installed("afex")
  skip_if_not_installed("tidyr")
  data(obk.long, package = "afex", envir = environment())
  fit <- suppressMessages(afex::aov_ez(
    "id", "value", obk.long,
    between = c("treatment", "gender"), within = c("phase", "hour")))
  md <- rt_to_markdown(as_spec(apa.afex.table(fit))$notes[[1]])
  expect_match(md, "*p*-values", fixed = TRUE)
  expect_false(grepl("[^*]p-values", md))
})
