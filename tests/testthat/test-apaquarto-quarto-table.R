# apa.quarto.pdf() end to end: the ladder's decision reaching the page.
#
# This is the only place several claims about the LaTeX path become assertable
# at all, because each of them is about what arrives in the PDF rather than
# what R returned:
#
#   1. The environment arrives UNSTARRED with its caption spliced INSIDE
#      it. The emitter deliberately writes no \caption, and Quarto rewrites a
#      captionless raw longtable to longtable*, which is invisible to every
#      later longtable fixup and silently loses rows (measured: 45 of 60 in one
#      document, 38 of 60 in another, compiling cleanly both times). Nothing
#      in R can check this; the caption is spliced by apaquarto, after R has
#      finished.
#
#   2. Rotation is AUTOMATIC. The wide table below carries NO
#      `::: {.landscape}` div, NO `#| apa-landscape` option, and NO argument.
#      R measures it, decides, and the decision reaches apaquarto as the
#      `apa-landscape` attribute apafloatlatex.lua reads.
#
#   3. Rotation happens ONLY where it is needed: the narrow table in the same
#      document stays upright. An "everything rotates" bug would satisfy every
#      rotation assertion on its own.
#
# Rotation is decided by RASTERIZING. /Rotate via `strings` reads 0 on a
# genuinely rotated PDF and pdfinfo's per-page report is no better; a page that
# comes out wider than it is tall is one a reader sees as landscape.
#
# Both placements are rendered: with floats collected at the end apa7's
# endfloat moves the environment into a .ttt file and drops what surrounded it,
# which is a different route and has failed independently before.

skip_on_cran()
skip_if_not(nzchar(Sys.which("quarto")), "quarto not installed")
skip_if_not(nzchar(Sys.which("pdftotext")), "poppler (pdftotext) not installed")
skip_if_not(nzchar(Sys.which("pdfinfo")), "poppler (pdfinfo) not installed")
skip_if_not(nzchar(Sys.which("pdftoppm")), "poppler (pdftoppm) not installed")

# quarto spawns its own R session and calls library(apaTables), so a render
# exercises the INSTALLED package -- its R code AND the extension it ships --
# and apa.quarto.pdf() is a new export, so a stale library does not merely
# certify the wrong thing here, it cannot run the document at all. The guard
# is in helper-stale-library.R.
apa_skip_if_library_stale()

# Row markers are letters only: an underscore in a cell is math mode and ABORTS
# the render, and a digit would collide with the correlation values.
#
# The two tables are chosen against the MEASURED width model, not by eye.
# mtcars[, 1:9] lays out at 6.889 in at 10 pt, which is past the 6.5 in upright
# text block and inside the 9 in rotated one, so the correct answer is "rotate
# and stay at 10 pt" -- rotate before shrink. attitude[, 1:3] lays out at
# 2.927 in and must not rotate.
apa_qtable_qmd <- function(placement) {
  paste0(
    "---\n",
    "title: \"Automatic Rotation\"\n",
    "shorttitle: \"Automatic Rotation\"\n",
    "author:\n",
    "  - name: Test Author\n",
    "    corresponding: true\n",
    "    email: test@example.org\n",
    "    affiliations:\n",
    "      - name: Test University\n",
    if (identical(placement, "intext")) "floatsintext: true\n" else "",
    "format:\n  apaquarto-pdf:\n    keep-tex: true\n",
    "---\n\n",
    "## Results\n\n",
    "XREF: @tbl-narrow, @tbl-wide\n\n",
    "```{r}\n",
    "#| label: tbl-narrow\n",
    "#| tbl-cap: \"A Narrow Table\"\n",
    "#| apa-note: \"NOTEMARKERaa belongs to the narrow table.\"\n",
    "#| echo: false\n",
    "#| message: false\n",
    "#| warning: true\n",
    "library(apaTables)\n",
    "d <- attitude[, 1:3]; names(d)[3] <- \"NARROWmarkeraa\"\n",
    "apa.cor.table(d) |> apa.quarto.pdf()\n",
    "```\n\n",
    "```{r}\n",
    "#| label: tbl-wide\n",
    "#| tbl-cap: \"A Wide Table\"\n",
    "#| apa-note: \"NOTEMARKERbb belongs to the wide table.\"\n",
    "#| echo: false\n",
    "#| message: false\n",
    "#| warning: true\n",
    "w <- mtcars[, 1:9]; names(w)[9] <- \"WIDEmarkerbb\"\n",
    "apa.cor.table(w) |> apa.quarto.pdf()\n",
    "```\n\n",
    "## Discussion\n\n",
    "ENDMARKER closes the body.\n")
}

for (placement in c("end", "intext")) {

  test_that(paste0("apa.quarto.pdf() rotates the wide table by itself and ",
                   "leaves the narrow one upright (", placement, ")"), {
    proj <- file.path(tempdir(), paste0("qtable-", placement))
    unlink(proj, recursive = TRUE)
    suppressMessages(apa.quarto.new(proj, filename = "doc.qmd"))
    qmd <- apa_qtable_qmd(placement)

    # The document must not be able to ask for what it is meant to receive.
    # A fixture that hands the code under test the answer certifies nothing --
    # e.g. an `apa-longtable` option satisfies the first half of a Lua
    # disjunction and the detection under test is never reached.
    expect_false(grepl("landscape", qmd, fixed = TRUE))
    expect_false(grepl("apa-landscape", qmd, fixed = TRUE))
    writeLines(qmd, file.path(proj, "doc.qmd"))

    log <- suppressWarnings(system2(
      "quarto", c("render", shQuote(file.path(proj, "doc.qmd"))),
      stdout = TRUE, stderr = TRUE))
    out <- file.path(proj, "doc.pdf")
    expect_true(file.exists(out), info = paste(log, collapse = "\n"))
    skip_if_not(file.exists(out), "render produced no output to read")

    # Each .tex is read in its own right. Under endfloat the .ttt holds a COPY
    # that endfloat re-emitted alone, dropping what surrounded it, so
    # concatenating the two hides a defect present in only one of them and
    # makes every "exactly one" count read two.
    # Where floats are collected at the end, doc.ttt MUST be there: endfloat
    # writes the deferred environment into it, and every end-placement
    # assertion below reads it. Without this the file list silently narrows to
    # doc.tex alone and the end placement is certified by the body .tex, which
    # under endfloat holds no table at all.
    if (identical(placement, "end")) {
      expect_true(file.exists(file.path(proj, "doc.ttt")),
                  info = "endfloat wrote no doc.ttt")
    }
    tex_files <- file.path(proj, c("doc.tex", "doc.ttt"))
    tex_files <- tex_files[file.exists(tex_files)]
    expect_true(length(tex_files) > 0L)

    for (tf in tex_files) {
      tex <- readLines(tf, warn = FALSE)
      where <- basename(tf)
      ib <- grep("\\begin{longtable}", tex, fixed = TRUE)
      ie <- grep("\\end{longtable}", tex, fixed = TRUE)

      expect_length(ib, 2L)
      expect_length(ie, 2L)
      expect_false(any(grepl("\\begin{longtable*}", tex, fixed = TRUE)),
                   info = paste(where, "-- an environment came out starred"))
      expect_false(any(grepl("\\begin{table}", tex, fixed = TRUE)),
                   info = paste(where, "-- a longtable is inside a float"))

      # The assertion this file exists to make possible: EACH caption is
      # spliced inside its OWN environment. The emitter writes no caption; if
      # apaquarto stopped splicing one, Quarto would star the environment and
      # rows would go missing with no error anywhere.
      for (lab in c("tbl-narrow", "tbl-wide")) {
        icap <- grep(paste0("\\caption{\\label{", lab, "}"), tex, fixed = TRUE)
        expect_length(icap, 1L)
        inside <- length(icap) == 1L &&
          any(icap > ib & icap < ie)
        expect_true(inside, info = paste(where, lab,
                                         "-- caption is not inside a longtable"))
      }

      # Rotation, in the .tex: exactly ONE literal landscape block -- the wide
      # table's. Two would mean the narrow table was rotated as well; zero
      # would mean the decision never crossed from R into apaquarto.
      expect_equal(sum(grepl("\\begin{landscape}", tex, fixed = TRUE)), 1L,
                   info = where)
      expect_equal(sum(grepl("\\end{landscape}", tex, fixed = TRUE)), 1L,
                   info = where)
      expect_false(any(grepl("sidewaystable", tex, fixed = TRUE)), info = where)
    }

    pages <- apa_pdf_pages(out)
    shapes <- apa_pdf_page_shapes(out)

    # Rotation, on the PAGE. Rasterized, because both cheap textual checks
    # have been measured lying about it.
    narrow_page <- apa_page_of(pages, "NARROWmarkeraa")
    wide_page <- apa_page_of(pages, "WIDEmarkerbb")
    expect_false(is.na(narrow_page))
    expect_false(is.na(wide_page))
    expect_equal(shapes[narrow_page], "portrait")
    expect_equal(shapes[wide_page], "landscape")
    expect_true(sum(shapes == "landscape") >= 1L)

    # Each note is on its own table's page, not merely somewhere in the file.
    expect_equal(apa_page_of(pages, "NOTEMARKERaa"), narrow_page)
    expect_equal(apa_page_of(pages, "NOTEMARKERbb"), wide_page)

    # The numbers, in the captions and in the body's own references.
    expect_equal(apa_pdf_caption_numbers(pages), 1:2)
    expect_equal(apa_xref_numbers(pages), 1:2)
  })
}

# ---------------------------------------------------------------------------
# C1: a decision to rotate that apaquarto CANNOT CARRY OUT.
#
# Rotation is executed by apafloatlatex.lua's landscapewrap, which wraps a
# FLOAT. Quarto builds a float out of a code cell only when its label begins
# `tbl-`, so in a chunk with no label -- or with a label of any other shape --
# there is nothing to wrap. apa_request_landscape() knew this and returned
# FALSE; the ladder did not, because the answer was discarded. The table was
# therefore priced against the 9 in rotated budget and printed on the 6.5 in
# upright page, and the columns past the right edge left the document.
#
# WATCHED FAILING, against the pre-fix build of this branch: this document
# rendered exit 0 with no warning, every page upright, and 3 of its 16 column
# headings absent from the PDF. The same shape reproduced on the plainest
# possible document -- `apa.cor.table(mtcars) |> apa.quarto.pdf()` in an
# unlabelled chunk -- which came out with 9 of its 10 correlation columns. On
# `master` this table was shrunk to the upright page and kept every column, so
# it was a REGRESSION, and of exactly the kind this phase exists to remove.
#
# The columns are checked BY NAME, not by counting: every heading is its own
# marker, so a column that fell off the page edge is a missing string rather
# than an arithmetic difference between two numbers this file computed itself.
APA_UNLABELLED_COLS <- 19L

# 19 marker-headed columns lay out at 8.836 in at 10 pt: past the 6.5 in
# upright text block, inside the 9 in rotated one -- so the ladder chooses to
# rotate -- and past the 7.5 in that is left of an 8.5 in sheet once the 1 in
# left margin is taken, so an unexecuted rotation pushes the right-hand
# columns clean OFF THE PAPER rather than merely into the margin. That last
# number is what makes the assertion below bite: pdftotext reports text that
# is outside the page but still in the content stream, so a table that only
# overflows the MARGIN reads as complete. Fitted upright instead the table
# comes to 6.96 pt and 6.436 in, which is observable in the .tex as well.
apa_unlabelled_marker <- function(j) {
  a <- "abcdefghijklmnopqrstuvwxyz"
  paste0("COL", substr(a, j, j), substr(a, j, j))
}

apa_unlabelled_qmd <- function(placement) {
  paste0(
    "---\n",
    "title: \"An Unlabelled Chunk\"\n",
    "shorttitle: \"Unlabelled\"\n",
    "author:\n",
    "  - name: Test Author\n",
    "    corresponding: true\n",
    "    email: test@example.org\n",
    "    affiliations:\n",
    "      - name: Test University\n",
    if (identical(placement, "intext")) "floatsintext: true\n" else "",
    "format:\n  apaquarto-pdf:\n    keep-tex: true\n",
    "---\n\n",
    "## Results\n\n",
    # NO label of any kind. That absence is the case under test, so it is
    # asserted of the document text below before anything is rendered.
    "```{r}\n",
    "#| echo: false\n",
    "#| message: false\n",
    "#| warning: true\n",
    "library(apaTables)\n",
    "a <- \"abcdefghijklmnopqrstuvwxyz\"\n",
    "nc <- ", APA_UNLABELLED_COLS, "; nr <- 4L\n",
    "sp <- apaTables:::apa_spec_start(nr)\n",
    "for (j in seq_len(nc)) sp <- apaTables:::apa_col(sp,\n",
    "  paste0(\"COL\", substr(a, j, j), substr(a, j, j)), \"r\",\n",
    "  sprintf(\"%s%02d\", tolower(substr(a, j, j)), seq_len(nr)))\n",
    "sp <- apaTables:::apa_spec_finish(sp, title = \"A Wide Table\")\n",
    "apa.quarto.pdf(sp)\n",
    "```\n\n",
    "## Discussion\n\nENDMARKER closes the body.\n")
}

for (placement in c("end", "intext")) {

  test_that(paste0("a wide table in an UNLABELLED chunk keeps every column (",
                   placement, ")"), {
    proj <- file.path(tempdir(), paste0("qtable-unlabelled-", placement))
    unlink(proj, recursive = TRUE)
    suppressMessages(apa.quarto.new(proj, filename = "doc.qmd"))
    qmd <- apa_unlabelled_qmd(placement)

    # The document must not be able to ask for what it is meant to receive --
    # and here it must not be able to ask for a label either, since the whole
    # case is the absence of one.
    expect_false(grepl("label:", qmd, fixed = TRUE))
    expect_false(grepl("landscape", qmd, fixed = TRUE))
    writeLines(qmd, file.path(proj, "doc.qmd"))

    log <- suppressWarnings(system2(
      "quarto", c("render", shQuote(file.path(proj, "doc.qmd"))),
      stdout = TRUE, stderr = TRUE))
    out <- file.path(proj, "doc.pdf")
    expect_true(file.exists(out), info = paste(log, collapse = "\n"))
    skip_if_not(file.exists(out), "render produced no output to read")

    pages <- apa_pdf_pages(out)
    doc <- paste(pages, collapse = "\n")

    # THE ASSERTION. Every column heading reached the page, compared as a SET
    # so that the failure names the columns that went missing.
    expected <- vapply(seq_len(APA_UNLABELLED_COLS), apa_unlabelled_marker,
                       character(1))
    found <- expected[vapply(expected, function(m) grepl(m, doc, fixed = TRUE),
                             logical(1))]
    expect_equal(sort(setdiff(expected, found)), character(0),
                 info = "column headings that never reached the page")

    # And it kept them by SHRINKING, not by rotating: with no float there is
    # nothing to rotate, so a landscape page here would mean the mechanism had
    # changed under the test rather than that the table was safe.
    shapes <- apa_pdf_page_shapes(out)
    expect_true(all(shapes == "portrait"),
                info = paste(shapes, collapse = " "))
    tex_files <- file.path(proj, c("doc.tex", "doc.ttt"))
    tex_files <- tex_files[file.exists(tex_files)]
    tex <- paste(unlist(lapply(tex_files, readLines, warn = FALSE)),
                 collapse = "\n")
    sizes <- as.numeric(unique(sub("^fontsize\\{([0-9.]+)\\}$", "\\1",
      regmatches(tex, gregexpr("fontsize\\{[0-9.]+\\}", tex))[[1]])))
    expect_true(length(sizes) > 0L)
    expect_true(all(sizes < APA_QUARTO_DEFAULT_SIZE),
                info = paste("font sizes:", paste(sizes, collapse = " ")))
    expect_false(any(grepl("\\begin{landscape}", tex, fixed = TRUE)))
  })
}
