# Calibration against reality.
#
# What survives here is the PAGE: the text block each kind of document gives a
# table (R/page-geometry.R), which is what the Phase 2 ladder measures against.
# A wrong page is a clipped table however good the width model is.
#
# The flextable width model and the height model were calibrated here too, and
# Task 10 deleted both -- the first superseded by R/render-latex.R's native
# model (calibrated in test-render-latex.R), the second removed outright
# because D3 made height cosmetic rather than data-critical. Their tests went
# with them.
#
# A test that checks a constant by recomputing it cannot fail: if apa7 changes
# its text block, model and test drift together and the suite stays green while
# the output goes wrong. So every assertion here has its two sides come from
# DIFFERENT places. One side is apaTables. The other is a document rendered
# through apaquarto, read back two ways:
#
#   * LaTeX printing \the\linewidth and \the\textheight from inside the
#     rendered document, for the page it really has;
#   * the .log of a xelatex pass over the kept .tex, for the "Overfull \hbox
#     (N pt too wide)" that is LaTeX's own measurement of how far a table
#     misses the page by.
#
# Maintainer-only, like the other render tests here: skipped on CRAN and
# wherever the toolchain (quarto + a LaTeX engine + poppler + the apaquarto
# extension sandbox + the apaTables source tree) is not present.

# ---- toolchain, render, and the two readings ---------------------------

cal_cache <- new.env(parent = emptyenv())

# Render a calibration fixture once per test session and keep the .tex, the
# PDF and a LaTeX log beside it. Returns NULL after calling skip() when the
# toolchain is incomplete.
cal_render <- function(fixture, base) {
  if (!is.null(cal_cache[[base]])) return(cal_cache[[base]])

  skip_on_cran()
  skip_if_not(nzchar(Sys.which("quarto")), "quarto CLI not available")
  skip_if_not(nzchar(Sys.which("pdftotext")), "pdftotext (poppler) not available")
  skip_if_not(nzchar(Sys.which("xelatex")), "xelatex not available")

  pkg <- Sys.getenv("APATABLES_PKG_ROOT",
                    normalizePath(test_path("..", ".."), mustWork = FALSE))
  # The extension apaTables SHIPS, not a sandbox on one machine. This used to
  # default to an absolute path under one developer's home directory, so
  # everywhere else the whole file skipped -- and this is the only file in the
  # suite carrying real LaTeX ground truth, which made its silence expensive.
  # The bundled copy is also the right one to calibrate against: it is the
  # extension a user renders with.
  ext <- Sys.getenv("APATABLES_APAQUARTO_EXT",
                    file.path(pkg, "inst", "_extensions"))
  # These fixtures pkgload::load_all() the SOURCE tree in a spawned quarto R
  # session, so they cannot run from a built copy (R CMD check) or from
  # covr's installed temp copy, where R/ holds no .R files.
  skip_if_not(file.exists(file.path(pkg, "DESCRIPTION")) &&
                dir.exists(file.path(pkg, "R")) &&
                length(list.files(file.path(pkg, "R"), pattern = "[.]R$")) > 0,
              "apaTables source tree not available (e.g. inside R CMD check or covr)")
  skip_if_not(dir.exists(ext), "apaquarto extension sandbox not available")
  Sys.setenv(APATABLES_PKG_ROOT = pkg)

  tmp <- tempfile("apaqcal"); dir.create(tmp)
  withr::defer(unlink(tmp, recursive = TRUE), testthat::teardown_env())
  file.copy(test_path("fixtures", fixture), file.path(tmp, paste0(base, ".qmd")))
  file.copy(ext, tmp, recursive = TRUE)

  out <- suppressWarnings(system2("quarto",
    c("render", file.path(tmp, paste0(base, ".qmd"))),
    stdout = TRUE, stderr = TRUE))
  pdf <- file.path(tmp, paste0(base, ".pdf"))
  tex <- file.path(tmp, paste0(base, ".tex"))
  expect_true(file.exists(pdf), info = paste(out, collapse = "\n"))
  expect_true(file.exists(tex), info = "keep-tex did not leave a .tex")
  if (!file.exists(pdf) || !file.exists(tex)) return(NULL)

  # One xelatex pass over the kept .tex, purely for its log: quarto does not
  # keep the log, and the overfull-box report in it is the only place LaTeX
  # states, in its own arithmetic, how wide a table came out.
  wd <- setwd(tmp); on.exit(setwd(wd), add = TRUE)
  system2("xelatex", c("-interaction=nonstopmode", basename(tex)),
          stdout = FALSE, stderr = FALSE)
  logf <- file.path(tmp, paste0(base, ".log"))

  res <- list(
    dir = tmp,
    tex = paste(readLines(tex, warn = FALSE), collapse = "\n"),
    text = paste(system2("pdftotext", c(pdf, "-"), stdout = TRUE),
                 collapse = "\n"),
    log = if (file.exists(logf)) readLines(logf, warn = FALSE) else character(0))

  # THE LOG IS PROVED REAL HERE, and the reason is that cal_overfull_pt()
  # returns numeric(0) two ways: when LaTeX reported no overfull row, and when
  # there is no log to report anything. The two overfull assertions below are
  # `expect_identical(..., numeric(0))`, so without this they PASS when
  # xelatex never ran -- the strongest evidence in the file, quietly reduced
  # to a check that a file is missing. "Output written on" is xelatex's own
  # statement that the pass reached the end of the document.
  expect_true(length(res$log) > 0L,
              info = "xelatex left no log, so the overfull-box assertions would pass vacuously")
  expect_true(any(grepl("Output written on", res$log, fixed = TRUE)),
              info = paste("the xelatex pass did not complete, so the",
                           "overfull-box assertions would pass vacuously"))

  assign(base, res, envir = cal_cache)
  res
}

# The font sizes each table in a .tex was written at, in document order.
cal_tex_fontsizes <- function(tex) {
  chunks <- strsplit(tex, "\\\\begin\\{longtable\\*?\\}")[[1]][-1]
  lapply(chunks, function(ch) {
    ch <- sub("\\\\end\\{longtable\\*?\\}.*", "", ch)
    m <- regmatches(ch, gregexpr("fontsize\\{[0-9.]+\\}", ch))[[1]]
    sort(unique(as.numeric(sub("^fontsize\\{([0-9.]+)\\}$", "\\1", m))))
  })
}

# The distinct "Overfull \hbox (N pt too wide) ... in alignment" amounts a
# LaTeX log reports: LaTeX's own statement of how far each table missed the
# line width by. "in alignment" keeps table rows and drops overfull lines of
# prose.
cal_overfull_pt <- function(log) {
  hit <- grep("Overfull .hbox .*in alignment", log, value = TRUE)
  if (!length(hit)) return(numeric(0))
  sort(unique(as.numeric(sub(".*\\(([0-9.]+)pt too wide\\).*", "\\1", hit))))
}

# A length LaTeX printed, e.g. "APALINEWIDTH=469.75502pt", as inches.
cal_reported_inches <- function(text, key) {
  m <- regmatches(text, regexpr(paste0(key, "=[0-9.]+pt"), text))
  if (!length(m)) return(NA_real_)
  as.numeric(sub(paste0(key, "=([0-9.]+)pt"), "\\1", m)) / 72.27
}

# What a document with this YAML asks apa_page_geometry() for, without any
# dependence on the R session being inside a render.
cal_meta_apaquarto <- list(format = list("apaquarto-pdf" = list(floatsintext = TRUE)))
cal_meta_jou <- list(format = list("apaquarto-pdf" = list(documentmode = "jou")))

# ---- the page ----------------------------------------------------------

test_that("the text block the model assumes is the text block LaTeX reports", {
  cal <- cal_render("apaquarto-calibration.qmd", "cal")
  skip_if(is.null(cal), "render failed; already reported")

  # LaTeX printed these itself, from inside the rendered document
  lw <- cal_reported_inches(cal$text, "APALINEWIDTH")
  th <- cal_reported_inches(cal$text, "APATEXTHEIGHT")
  expect_false(is.na(lw))
  expect_false(is.na(th))

  g <- apa_page_geometry("portrait", meta = cal_meta_apaquarto,
                         twocolumn = FALSE)
  expect_equal(g$width, lw, tolerance = 0.002)
  expect_equal(g$height, th, tolerance = 0.002)
  expect_identical(g$config, "apaquarto-man-letter")

  # a rotated page is the same block turned through 90 degrees
  gl <- apa_page_geometry("landscape", meta = cal_meta_apaquarto,
                          twocolumn = FALSE)
  expect_equal(gl$width, th, tolerance = 0.002)
  expect_equal(gl$height, lw, tolerance = 0.002)

  # End to end, and the assertion LaTeX itself makes: whatever the ladder
  # decided for the wide table and the narrow one, xelatex reports no overfull
  # row for either. Both sides are independent -- apaTables chose the layout,
  # LaTeX measured the result -- which is what the deleted width-model tests
  # were for. This one is stronger, because it is about the table on the page
  # rather than about a constant inside the model.
  expect_identical(cal_overfull_pt(cal$log), numeric(0))
})

test_that("a journal column is as narrow as the model says, and a table laid out for it does not overflow", {
  cal <- cal_render("apaquarto-calibration-jou.qmd", "caljou")
  skip_if(is.null(cal), "render failed; already reported")

  # LaTeX's own width for the environment apaquarto puts a table in
  lw <- cal_reported_inches(cal$text, "APAJOULINEWIDTH")
  expect_false(is.na(lw))
  expect_lt(lw, 3.5)                     # a journal column, not a page

  g <- apa_page_geometry("portrait", meta = cal_meta_jou, twocolumn = FALSE)
  expect_equal(g$width, lw, tolerance = 0.002)
  expect_identical(g$config, "apaquarto-jou")

  # end to end: the table was set small enough for the column it is in, and
  # xelatex reports no overfull row. Laid out for 6.5 in -- what the model
  # assumed before it read documentmode -- this same table overfilled the
  # column by 195.58 pt and lost two of its three correlation columns off the
  # page.
  expect_identical(cal_overfull_pt(cal$log), numeric(0))
  sizes <- cal_tex_fontsizes(cal$tex)
  expect_length(sizes, 1L)
  # smaller than the ladder's own default, which is already smaller than 12
  expect_lt(max(sizes[[1]]), APA_QUARTO_DEFAULT_SIZE)
})

