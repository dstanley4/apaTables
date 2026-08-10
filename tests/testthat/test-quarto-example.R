tmpl_path <- function(name) system.file("templates", name, package = "apaTables")

test_that("both example templates ship and are complete", {
  for (nm in c("example-pdf.qmd", "example-docx.qmd")) {
    template <- tmpl_path(nm)
    expect_true(nzchar(template) && file.exists(template), info = nm)
    txt <- paste(readLines(template, warn = FALSE), collapse = "\n")
    lines <- readLines(template, warn = FALSE)
    # every table type present, in both variants
    for (fn in c("apa.cor.table", "apa.reg.table", "apa.d.table",
                 "apa.aov.table", "apa.1way.table", "apa.2way.table",
                 "apa.afex.table")) expect_match(txt, fn, fixed = TRUE, info = nm)
    # ONE PAGE PER TABLE, asserted as that invariant rather than as a count.
    # This used to read `expect_identical(..., 7L)` for both templates, and
    # Phase 3.3 added a NINTH page to the apaquarto variant (the wide
    # correlation table on a rotated page, D102) -- at which point a hard
    # number fails for a document that is still correct, and the fix is to
    # bump a magic constant rather than to check anything. Derived from the
    # table chunks instead, so the two templates are allowed to differ in
    # length while both stay one-page-per-table.
    n_tbl <- lengths(regmatches(txt, gregexpr("#\\| label: tbl-", txt)))
    expect_identical(
      lengths(regmatches(txt, gregexpr("{{< pagebreak >}}", txt, fixed = TRUE))),
      n_tbl - 1L, info = nm)
    # the afex page always evaluates and carries the visible install reminder
    expect_no_match(txt, "eval: false", fixed = TRUE)
    expect_match(txt, 'install.packages(c("afex", "tidyr"))', fixed = TRUE)
    # every TABLE chunk (label: tbl-*, i.e. excluding the setup chunk)
    # teaches message: false / warning: true; warning is NEVER taught as
    # false (it is apaTables' advisory channel)
    n_table_chunks <- lengths(regmatches(txt, gregexpr("#\\| label: tbl-", txt)))
    expect_identical(sum(lines == "#| message: false"), n_table_chunks, info = nm)
    expect_identical(sum(lines == "#| warning: true"), n_table_chunks, info = nm)
    expect_no_match(txt, "warning: false", fixed = TRUE)
    # BOTH variants use apaquarto (the axis the twins differ on is the
    # render target, not the extension -- the plain-Quarto route is
    # documented in the vignette instead of shipped as a third file), so
    # both set floatsintext and carry NOT ONE landscape marker: the wide
    # tables reach rotated pages because R measures them and asks the
    # bundled extension (rendered and rasterized in
    # test-apaquarto-integration.R, which is what stops this from being a
    # decorative absence check).
    expect_match(txt, "floatsintext: true", fixed = TRUE, info = nm)
    expect_identical(sum(lines == "::: {.landscape}"), 0L, info = nm)
    expect_identical(sum(lines == ":::"), 0L, info = nm)
    # apa.quarto.example() installs the extension itself (via
    # apa.quarto.setup()) before these files are ever opened, so the comment
    # must say so and must NOT send the reader to "quarto add"/"quarto use
    # template" -- those commands install the stock extension and, run in a
    # folder that already has apaTables' patched copy, silently revert it.
    expect_match(txt, "already installed", fixed = TRUE, info = nm)
    expect_match(txt, "apa.quarto.setup()", fixed = TRUE, info = nm)
    expect_no_match(txt, "quarto use template", fixed = TRUE)
    expect_no_match(txt, "quarto add wjschne/apaquarto", fixed = TRUE)
  }
  # The variant-specific invariant: format line and entry-point calls agree
  # WITHIN each file (the mismatch guidance line apa_quarto_dispatch()
  # would otherwise print has no business in a document the package itself
  # wrote), and each file names its twin.
  pdf_txt <- paste(readLines(tmpl_path("example-pdf.qmd"), warn = FALSE), collapse = "\n")
  expect_match(pdf_txt, "apaquarto-pdf:", fixed = TRUE)
  expect_match(pdf_txt, "apa.quarto.pdf(my_table", fixed = TRUE)
  expect_no_match(pdf_txt, "apa.quarto.docx(my_table", fixed = TRUE)
  expect_match(pdf_txt, "-docx.qmd", fixed = TRUE)
  docx_txt <- paste(readLines(tmpl_path("example-docx.qmd"), warn = FALSE), collapse = "\n")
  expect_match(docx_txt, "apaquarto-docx:", fixed = TRUE)
  expect_match(docx_txt, "apa.quarto.docx(my_table", fixed = TRUE)
  expect_no_match(docx_txt, "apa.quarto.pdf(my_table", fixed = TRUE)
  expect_match(docx_txt, "-pdf.qmd", fixed = TRUE)
})

test_that("apa.quarto.example writes BOTH files from a stem and returns their paths", {
  stem <- file.path(tempdir(), "apaq-twin.qmd")
  f_pdf <- file.path(tempdir(), "apaq-twin-pdf.qmd")
  f_docx <- file.path(tempdir(), "apaq-twin-docx.qmd")
  on.exit(unlink(c(f_pdf, f_docx)), add = TRUE)
  unlink(c(f_pdf, f_docx))

  res <- suppressMessages(apa.quarto.example(filename = stem))
  expect_identical(res, c(pdf = f_pdf, docx = f_docx))
  expect_true(file.exists(f_pdf) && file.exists(f_docx))
  expect_identical(readLines(f_pdf, warn = FALSE),
                   readLines(tmpl_path("example-pdf.qmd"), warn = FALSE))
  expect_identical(readLines(f_docx, warn = FALSE),
                   readLines(tmpl_path("example-docx.qmd"), warn = FALSE))
})

test_that("apa.quarto.example guards overwrite, parent dir, and bad input", {
  stem <- file.path(tempdir(), "apaq-twin-guard.qmd")
  f_pdf <- file.path(tempdir(), "apaq-twin-guard-pdf.qmd")
  f_docx <- file.path(tempdir(), "apaq-twin-guard-docx.qmd")
  on.exit(unlink(c(f_pdf, f_docx)), add = TRUE)
  unlink(c(f_pdf, f_docx))

  suppressMessages(apa.quarto.example(filename = stem))
  expect_error(apa.quarto.example(filename = stem), "already exists")
  expect_identical(
    unname(suppressMessages(apa.quarto.example(filename = stem, overwrite = TRUE))),
    c(f_pdf, f_docx))
  expect_error(apa.quarto.example(filename = file.path(tempdir(), "no-such-dir-apaq", "x.qmd")),
               "does not exist")
  expect_error(apa.quarto.example(filename = c("a", "b")), "single file path")
})
