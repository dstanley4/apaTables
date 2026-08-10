# Special characters in variable names must not break apaTables, and must be
# rendered FAITHFULLY (verbatim) in every modern output path. apaTables is a
# formatter: it should display whatever name the user supplied, unchanged.
#
# THIS FILE COVERS the spec, which feeds apa_flextable() -> Word (.docx) and
# HTML. It used to declare the LaTeX path OUT OF SCOPE, and Phase 2 makes that
# exclusion wrong: LaTeX is now a first-class destination, reached by
# apa.quarto.pdf() in every Quarto -> PDF render. The other two halves are
# covered in the files that own those layers, from the SAME `tricky_names`
# vector (tests/testthat/helper-special-names.R):
#
#   test-render-latex.R          a real builder spec through
#                                apa_display_plan() -> apa_latex_table(), with
#                                the exact escaped LaTeX asserted per name
#   test-phase2-acceptance.R     a real Quarto render, both placements, with
#                                every name looked for in the PDF's own text
#
# The legacy `$latex.*` fields intentionally rewrite some characters
# (e.g. "_" -> " ") and remain out of scope. They fed apa.knit.table.for.pdf(),
# which was REMOVED in 4.0; the builders still populate them and nothing in the
# package reads them any more.

# The first column of each spec body row holds the variable label, e.g.
# "1. my_var". Flatten the richtext cells to plain text.
spec_row_labels <- function(x) {
  vapply(x$spec$body, function(row) as_plaintext(row[[1]]), character(1))
}

# `tricky_names`, `tricky_names_no_formula` and `tricky_cor_df()` come from
# helper-special-names.R -- one list, three files, no second copy to drift.
cor_df <- tricky_cor_df

for (case_name in names(tricky_names)) {
  nm <- tricky_names[[case_name]]

  test_that(paste0("apa.cor.table preserves a '", case_name,
                   "' character in a variable name"), {
    ct <- apa.cor.table(cor_df(nm))

    # 1. modern render paths must not error
    expect_no_error(apa_flextable(ct))
    out <- tempfile(fileext = ".docx")
    expect_no_error(apa.save(filename = out, ct))
    expect_true(file.exists(out))
    unlink(out)

    # 2. the name must survive VERBATIM into the spec (-> flextable / .docx)
    labels <- spec_row_labels(ct)
    expect_true(
      any(grepl(nm, labels, fixed = TRUE)),
      info = paste0("'", nm, "' not found verbatim in spec labels: ",
                    paste(labels, collapse = " | "))
    )
  })
}


# A simple regression whose single predictor carries the tricky name. R wraps
# non-syntactic names in backticks in the coefficient label, so build the model
# with reformulate() + a backticked term.
reg_fit <- function(nm) {
  set.seed(1)
  d <- data.frame(crit = rnorm(40), p = rnorm(40))
  names(d)[2] <- nm
  stats::lm(stats::reformulate(paste0("`", nm, "`"), response = "crit"), data = d)
}

# WHY FOUR NAMES ARE EXCLUDED BELOW, AND WHY THAT IS PINNED RATHER THAN
# ASSERTED. A backslash, a newline or a tab cannot reach an lm() coefficient
# label at all: R's own deparser backquotes a non-syntactic name and ESCAPES
# the character on the way in, so the coefficient comes out named `a\\b` --
# backslash then "b", two characters -- and the name apaTables is handed is
# already not the name the user typed. Skipping them silently would read as
# apaTables failing to carry them, so the fact is measured here instead. The
# day R changes this, the test fails and the exclusion can go.
test_that("R itself -- not apaTables -- is what mangles these names in a formula", {
  # Two different ways R refuses the name, both measured: a backslash makes
  # lm() fail outright ("object 'a\b' not found", because the deparsed
  # backquoted term is re-parsed and the escape is consumed), while a newline
  # or a tab is accepted and silently rewritten in the coefficient label.
  carried <- function(nm) {
    tryCatch(any(grepl(nm, names(stats::coef(reg_fit(nm))), fixed = TRUE)),
             error = function(e) FALSE)
  }
  for (case_name in tricky_names_no_formula) {
    expect_false(carried(tricky_names[[case_name]]),
                 info = paste0("R now carries '", case_name,
                               "' into a coefficient label; drop it from ",
                               "tricky_names_no_formula and let the sweep ",
                               "below cover it"))
  }
  # The companion that shows the check can fail: a name R DOES carry.
  expect_true(carried(tricky_names[["ampersand"]]))
})

for (case_name in setdiff(names(tricky_names), tricky_names_no_formula)) {
  nm <- tricky_names[[case_name]]

  test_that(paste0("apa.reg.table preserves a '", case_name,
                   "' character in a predictor name"), {
    rt <- apa.reg.table(reg_fit(nm))

    # 1. modern render paths must not error
    expect_no_error(apa_flextable(rt))
    out <- tempfile(fileext = ".docx")
    expect_no_error(apa.save(filename = out, rt))
    expect_true(file.exists(out))
    unlink(out)

    # 2. the name must survive VERBATIM into the spec (-> flextable / .docx)
    labels <- spec_row_labels(rt)
    expect_true(
      any(grepl(nm, labels, fixed = TRUE)),
      info = paste0("'", nm, "' not found verbatim in spec labels: ",
                    paste(labels, collapse = " | "))
    )
  })
}
