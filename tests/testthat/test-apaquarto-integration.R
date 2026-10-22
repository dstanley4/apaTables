# End-to-end apaquarto tests: render each builder through apaquarto to PDF
# (the format researchers actually submit; LaTeX is also where complex
# structures -- decked-head spanners, multi-line cells, styled super/subscripts
# -- and special characters are most likely to break). Maintainer-only: these
# skip unless the full toolchain is present (quarto + a LaTeX engine + pdftotext
# + the apaquarto extension sandbox + the apaTables source tree).
#
# The fixtures call apa.quarto.pdf() -- the native LaTeX emitter every real
# apaquarto-pdf document uses -- never apa_flextable(). CONSEQUENCE:
# flextable's own output (what apa.save() actually writes) has no end-to-end
# rendered coverage through a real toolchain in ANY test file -- only
# unit-level coverage (test-render-flextable.R and friends, which build a
# flextable object and inspect it directly, never through Word or
# LibreOffice) and test-apa-save.R's docx-structure checks.

# Resolve apaquarto paths from env vars (with sane defaults) and export
# APATABLES_PKG_ROOT so the spawned quarto R session can load_all().
apaq_paths <- function() {
  pkg <- Sys.getenv("APATABLES_PKG_ROOT",
                    normalizePath(test_path("..", ".."), mustWork = FALSE))
  # The extension apaTables ships, which is the one a user renders with. Set
  # APATABLES_APAQUARTO_EXT to an `_extensions` folder to render against a
  # different copy (for example stock apaquarto).
  ext <- Sys.getenv("APATABLES_APAQUARTO_EXT",
                    file.path(pkg, "inst", "_extensions"))
  # These tests pkgload::load_all() the SOURCE tree inside a spawned quarto R
  # session. Skip when that source tree is not available -- e.g. inside
  # R CMD check / devtools::check(), where tests run from a built copy and `pkg`
  # resolves to the .Rcheck dir (no source DESCRIPTION/R), or under covr, where
  # `pkg` resolves to covr's temp INSTALLED copy (DESCRIPTION + R/ exist, but
  # R/ holds only binary .rdb/.rds -- no .R source files, so load_all() fails
  # in the spawned session). They still run under devtools::test() / pkgload
  # in the source tree.
  skip_if_not(file.exists(file.path(pkg, "DESCRIPTION")) &&
                dir.exists(file.path(pkg, "R")) &&
                length(list.files(file.path(pkg, "R"), pattern = "[.]R$")) > 0,
              "apaTables source tree not available (e.g. inside R CMD check or covr)")
  Sys.setenv(APATABLES_PKG_ROOT = pkg)
  list(ext = ext, pkg = pkg)
}

# Render a fixture through apaquarto to PDF; assert the PDF compiled (a LaTeX
# break -> no PDF) and return the extracted text for content assertions.
render_apaquarto_pdf <- function(fixture, base, return_pdf_path = FALSE) {
  skip_on_cran()
  skip_if_not(nzchar(Sys.which("quarto")), "quarto CLI not available")
  skip_if_not(nzchar(Sys.which("pdftotext")), "pdftotext (poppler) not available")
  has_latex <- (requireNamespace("tinytex", quietly = TRUE) && tinytex::is_tinytex()) ||
    nzchar(Sys.which("xelatex")) || nzchar(Sys.which("pdflatex"))
  skip_if_not(has_latex, "no LaTeX engine available for PDF rendering")

  p <- apaq_paths()
  skip_if_not(dir.exists(p$ext), "apaquarto extension sandbox not available")

  tmp <- tempfile("apaqpdf"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  file.copy(test_path("fixtures", fixture), file.path(tmp, paste0(base, ".qmd")))
  file.copy(p$ext, tmp, recursive = TRUE)

  res <- suppressWarnings(system2("quarto",
    c("render", file.path(tmp, paste0(base, ".qmd"))),
    stdout = TRUE, stderr = TRUE))
  pdf <- file.path(tmp, paste0(base, ".pdf"))
  expect_true(file.exists(pdf), info = paste(res, collapse = "\n"))
  if (!file.exists(pdf)) return("")
  if (return_pdf_path) {
    # caller inspects the PDF itself; keep the temp dir alive for them
    on.exit()                    # cancel the unlink registered above
    return(pdf)
  }
  paste(system2("pdftotext", c(pdf, "-"), stdout = TRUE), collapse = "\n")
}

test_that("a hand-built spec renders through apaquarto to PDF", {
  txt <- render_apaquarto_pdf("apaquarto-smoke.qmd", "smoke")
  expect_match(txt, "Synthetic Demonstration Table", fixed = TRUE)
  expect_match(txt, "Adverts", fixed = TRUE)
})

test_that("a regression table renders through apaquarto to PDF", {
  txt <- render_apaquarto_pdf("apaquarto-regression.qmd", "reg")
  expect_match(txt, "adverts", fixed = TRUE)     # predictor cell
  expect_match(txt, "airplay", fixed = TRUE)
  expect_match(txt, "Model 1", fixed = TRUE)     # row-group label rendered per block
})

test_that("a correlation table renders through apaquarto to PDF", {
  txt <- render_apaquarto_pdf("apaquarto-correlation.qmd", "cor")
  expect_match(txt, "rating", fixed = TRUE)      # a variable name
  expect_match(txt, "complaints", fixed = TRUE)
  expect_match(txt, "[", fixed = TRUE)           # a CI bracket -> multi-line cell rendered
})

test_that("a two-way table renders through apaquarto to PDF (decked head)", {
  txt <- render_apaquarto_pdf("apaquarto-twoway.qmd", "tw")
  expect_match(txt, "Female", fixed = TRUE)      # body row label
  expect_match(txt, "Male", fixed = TRUE)
  expect_match(txt, "None", fixed = TRUE)        # level-1 spanner (alcohol level)
  expect_match(txt, "Pints", fixed = TRUE)
  expect_match(txt, "alcohol", fixed = TRUE)     # level-2 spanner (factor name)
})

test_that("a one-way table renders through apaquarto to PDF", {
  txt <- render_apaquarto_pdf("apaquarto-oneway.qmd", "ow")
  expect_match(txt, "Placebo", fixed = TRUE)
  expect_match(txt, "High Dose", fixed = TRUE)
})

test_that("an aov table renders through apaquarto to PDF", {
  txt <- render_apaquarto_pdf("apaquarto-aov.qmd", "aov")
  expect_match(txt, "dose", fixed = TRUE)
  expect_match(txt, "Error", fixed = TRUE)
  expect_false(grepl("(Intercept)", txt, fixed = TRUE))   # suppressed row
})

test_that("an afex table renders through apaquarto to PDF", {
  skip_if_not_installed("afex")
  txt <- render_apaquarto_pdf("apaquarto-afex.qmd", "afex")
  expect_match(txt, "gender", fixed = TRUE)
  expect_match(txt, "alcohol", fixed = TRUE)
})

test_that("a d-value table renders through apaquarto to PDF (multi-line cells)", {
  txt <- render_apaquarto_pdf("apaquarto-dvalue.qmd", "dval")
  expect_match(txt, "Placebo", fixed = TRUE)
  expect_match(txt, "[", fixed = TRUE)           # a CI bracket -> multi-line cell
})

test_that("a two-way CI table renders through apaquarto to PDF (row groups)", {
  txt <- render_apaquarto_pdf("apaquarto-twoway-ci.qmd", "twci")
  expect_match(txt, "alcohol: None", fixed = TRUE)   # row-group label rendered
  expect_match(txt, "[", fixed = TRUE)               # a CI bracket
})

# The fixture wraps its chunk in ::: {.landscape} markers and passes no
# rotation argument. What is asserted is a documented promise: a document that
# ALREADY wraps a chunk in the markers keeps rendering with a rotated page.
# It tests the MARKERS even against the bundled extension, whose automatic
# rotation is live: the table is a single-model regression, which never
# rotates on its own (measured: the same fixture without the markers renders
# with no rotated page). Automatic rotation is tested in
# test-landscape-matrix.R and friends.
test_that("template-style landscape markers produce a rotated PDF page", {
  skip_if_not(nzchar(Sys.which("pdfinfo")), "pdfinfo (poppler) not available")
  pdf <- render_apaquarto_pdf("apaquarto-landscape.qmd", "landscape",
                              return_pdf_path = TRUE)
  skip_if_not(nzchar(pdf), "render failed; failure already reported by expect_true")
  on.exit(unlink(dirname(pdf), recursive = TRUE), add = TRUE)
  info <- system2("pdfinfo", c("-f", "1", "-l", "20", pdf), stdout = TRUE)
  rots <- grep("rot:\\s*90", info, value = TRUE)
  expect_gt(length(rots), 0)
  # and the rightmost column header actually made it onto the page. The
  # fixture's columns are Predictor | b | 95% CI | beta | 95% CI |
  # Unique R2 | 95% CI | r | Fit, so "CI" is columns 3, 5 and 7 of 9 and
  # matches with the right half of the table clipped away. "Fit" is the
  # last column, and it is the branch's headline claim -- a rotated page
  # holding a table laid out for it -- that this asserts.
  txt <- paste(system2("pdftotext", c(pdf, "-"), stdout = TRUE), collapse = "\n")
  expect_match(txt, "Fit", fixed = TRUE)

  # This fixture is a MIXED case -- it sets tbl-cap and no apa-note. Whether
  # the generated note appears depends on which extension it renders against:
  # the bundled one places it (apatablemeta.lua), stock apaquarto has no
  # filter to place it. So nothing here asserts the note either way.
  #
  # Two things hold against either extension, and both are asserted:
  #
  #   * the document's own tbl-cap survived. R emits a title of its own on
  #     every LaTeX render, and a mechanism that overwrote a caption the user
  #     wrote would be the single worst outcome.
  #   * the ::: {.apa-table-meta} div that carries the generated title and note
  #     printed NOTHING. With no filter present (stock apaquarto, or plain
  #     Quarto) the div survives all the way into pandoc's output, which is
  #     where the carrier could reach a finished page.
  #
  # The other half of the mixed case, the generated note actually arriving on a
  # ROTATED page, needs the bundled extension and is covered against it in
  # tests/testthat/test-title-note-defaults.R ("rotated mixed case"), in both
  # placements.
  expect_match(txt, "Multiple Regression Predicting Sales", fixed = TRUE)
  for (leak in c("apa-table-meta", "apa-title=", "apa-note="))
    expect_false(grepl(leak, txt, fixed = TRUE), info = leak)
})

test_that("the PDF example renders warning-free with rotated pages", {
  skip_if_not(nzchar(Sys.which("pdfinfo")), "pdfinfo (poppler) not available")
  skip_if_not_installed("afex"); skip_if_not_installed("tidyr")
  p <- apaq_paths()
  skip_if_not(dir.exists(p$ext), "apaquarto extension sandbox not available")

  tmp <- tempfile("apaqex"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  suppressMessages(apa.quarto.example(file.path(tmp, "ex.qmd")))
  # NO file.copy() of p$ext over the extension apa.quarto.example() just
  # installed, and that omission is load-bearing. The example document carries
  # no ::: {.landscape} markers: its one wide table -- the blocks regression,
  # 7.12 in -- reaches a rotated page only because R measures it and the
  # BUNDLED apaquarto acts on the request. Overwriting the bundle with stock
  # apaquarto makes the request inert and the rot: 90 assertion below fail,
  # which is the right answer for stock and the wrong document to ask it of:
  # what a user gets from apa.quarto.example() is the bundle, so that is what
  # is rendered here.
  qmd <- file.path(tmp, "ex-pdf.qmd")
  # point the document at the dev package, not the installed copy
  txt <- readLines(qmd, warn = FALSE)
  txt <- sub("^library\\(apaTables\\)$",
             sprintf('pkgload::load_all("%s", quiet = TRUE)', p$pkg), txt)
  writeLines(txt, qmd)
  res <- suppressWarnings(system2("quarto", c("render", qmd),
                                  stdout = TRUE, stderr = TRUE))
  pdf <- file.path(tmp, "ex-pdf.pdf")
  expect_true(file.exists(pdf), info = paste(res, collapse = "\n"))
  ptxt <- paste(system2("pdftotext", c(pdf, "-"), stdout = TRUE), collapse = "\n")
  expect_no_match(ptxt, "Warning", fixed = TRUE)   # "it should just work"
  expect_no_match(ptxt, "Registered S3", fixed = TRUE)
  expect_match(ptxt, "Table 8", fixed = TRUE)      # afex page rendered a real table
  info <- system2("pdfinfo", c("-f", "1", "-l", "30", pdf), stdout = TRUE)
  expect_gt(length(grep("rot:\\s*90", info)), 0)
})

test_that("the Word example renders through apaquarto-docx with a rotated page", {
  skip_if_not_installed("afex"); skip_if_not_installed("tidyr")
  p <- apaq_paths()
  skip_if_not(dir.exists(p$ext), "apaquarto extension sandbox not available")
  skip_on_cran()
  skip_if_not(nzchar(Sys.which("quarto")), "quarto CLI not available")

  tmp <- tempfile("apaqexdocx"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  suppressMessages(apa.quarto.example(file.path(tmp, "ex.qmd")))
  qmd <- file.path(tmp, "ex-docx.qmd")
  txt <- readLines(qmd, warn = FALSE)
  txt <- sub("^library\\(apaTables\\)$",
             sprintf('pkgload::load_all("%s", quiet = TRUE)', p$pkg), txt)
  writeLines(txt, qmd)
  res <- suppressWarnings(system2("quarto", c("render", qmd),
                                  stdout = TRUE, stderr = TRUE))
  docx <- file.path(tmp, "ex-docx.docx")
  expect_true(file.exists(docx), info = paste(res, collapse = "\n"))
  xml_dir <- file.path(tmp, "unzipped")
  utils::unzip(docx, files = "word/document.xml", exdir = xml_dir)
  xml <- paste(readLines(file.path(xml_dir, "word", "document.xml"),
                         warn = FALSE), collapse = "\n")
  # the wide tables reached rotated pages (landscape US letter section)
  expect_match(xml, 'w:orient="landscape"', fixed = TRUE)
  # table content made it through (regression predictor row label)
  expect_match(xml, "adverts", fixed = TRUE)
  # no advisory leaked into the manuscript body
  expect_no_match(xml, "Registered S3", fixed = TRUE)
})

# The plain-Quarto route is SUPPORTED -- an ordinary `format: pdf` document with
# no extension renders apaTables table bodies, with tbl-cap captions and
# hand-placed ::: {.landscape} markers. The route now lives in the vignette
# ("Using apaTables tables in an ordinary Quarto document"); this test is
# that vignette section, executed.
test_that("apaTables tables render in an ordinary Quarto document (no apaquarto)", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.which("quarto")), "quarto CLI not available")
  skip_if_not(nzchar(Sys.which("pdftotext")), "pdftotext (poppler) not available")
  skip_if_not(nzchar(Sys.which("pdfinfo")), "pdfinfo (poppler) not available")
  has_latex <- (requireNamespace("tinytex", quietly = TRUE) && tinytex::is_tinytex()) ||
    nzchar(Sys.which("xelatex")) || nzchar(Sys.which("pdflatex"))
  skip_if_not(has_latex, "no LaTeX engine available for PDF rendering")

  pkg <- apaq_paths()$pkg
  tmp <- tempfile("plainq"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  qmd <- file.path(tmp, "plain.qmd")
  writeLines(c(
    "---",
    'title: "Ordinary Quarto document"',
    "format: pdf",
    "geometry:",
    "  - margin=1in",
    "---",
    "",
    "```{r}",
    "#| include: false",
    sprintf('pkgload::load_all("%s", quiet = TRUE)', pkg),
    "```",
    "",
    "```{r}",
    "#| label: tbl-correlation",
    '#| tbl-cap: "Descriptive Statistics and Correlations"',
    "#| message: false",
    "#| warning: true",
    "my_table <- apa.cor.table(attitude)",
    "apa.quarto.pdf(my_table)",
    "```",
    "",
    "::: {.landscape}",
    "",
    "```{r}",
    "#| label: tbl-regblocks",
    '#| tbl-cap: "Hierarchical Multiple Regression Predicting Sales"',
    "#| message: false",
    "#| warning: true",
    "blkA <- lm(sales ~ adverts + airplay, data = album)",
    "blkB <- lm(sales ~ adverts + airplay + I(adverts * airplay), data = album)",
    "my_table <- apa.reg.table(blkA, blkB)",
    "apa.quarto.pdf(my_table)",
    "```",
    "",
    ":::"), qmd)
  res <- suppressWarnings(system2("quarto", c("render", qmd),
                                  stdout = TRUE, stderr = TRUE))
  pdf <- file.path(tmp, "plain.pdf")
  expect_true(file.exists(pdf), info = paste(res, collapse = "\n"))
  ptxt <- paste(system2("pdftotext", c(pdf, "-"), stdout = TRUE), collapse = "\n")
  expect_no_match(ptxt, "Warning", fixed = TRUE)
  expect_no_match(ptxt, "Registered S3", fixed = TRUE)
  # Quarto's own caption style, both tables present
  expect_match(ptxt, "Table 1", fixed = TRUE)
  expect_match(ptxt, "Table 2", fixed = TRUE)
  # the hand-placed markers turned the blocks-regression page
  info <- system2("pdfinfo", c("-f", "1", "-l", "10", pdf), stdout = TRUE)
  expect_gt(length(grep("rot:\\s*90", info)), 0)
})
