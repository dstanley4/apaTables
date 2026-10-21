# Extraction helpers for the apaquarto acceptance matrix
# (tests/testthat/test-apaquarto-acceptance.R).
#
# Everything here reads values back OUT of a rendered artefact. Nothing is
# inferred from the source document: the whole point of the matrix is that a
# claim about placement, numbering or note attachment is only worth as much as
# the page of the PDF it can be read off.

# apaquarto sets the space inside "Table 1" as a NON-BREAKING space (U+00A0),
# which is right for typesetting and wrong for matching: perl's \s does not
# match it and neither does a literal " " in a lookbehind, so every caption and
# every cross-reference reads as zero numbers found -- an empty result that
# compares unequal to 1:3 and so fails LOUDLY rather than silently, but for the
# wrong reason. Normalising it here keeps the assertions written the way they
# are meant to be read.
# Quarto writes ANSI colour escapes into the stream even when it is not a TTY,
# so a captured log line reads "\033[31mERROR (...)" and never "ERROR (...)".
# An anchored match on the raw log is therefore constant-FALSE -- measured on a
# document producing two genuine apaquarto ERROR lines, captured exactly as the
# tests capture it: 2 lines contained ERROR, a PDF was still produced, and
# grepl("^ERROR", log) found nothing. Strip the escapes before matching
# anything anchored, always.
apa_strip_ansi <- function(x) gsub("\033\\[[0-9;]*m", "", x)

apa_nbsp_to_space <- function(x) gsub("\u00a0", " ", x, fixed = TRUE)

# Page-by-page text of a PDF. -layout preserves columns so a note can be
# distinguished from a table row; per-page extraction is what makes
# "the note is on the same page as its table" checkable.
apa_pdf_npages <- function(pdf) {
  info <- paste(system2("pdfinfo", shQuote(pdf), stdout = TRUE), collapse = " ")
  as.integer(sub(".*Pages:\\s*([0-9]+).*", "\\1", info))
}

# Each page's /Rotate entry, in page order.
#
# Kept as a cross-check, NEVER as the assertion. Both of the cheap textual
# ways of asking a PDF whether it is rotated have been measured lying: on a
# genuinely rotated document `strings file.pdf | grep -c /Rotate` returned 0
# (the page tree is compressed, so strings cannot see it), and pdfinfo's
# per-page report was no better. Rotation is decided by
# apa_pdf_page_shapes(), which rasterizes.
apa_pdf_rotations <- function(pdf) {
  n <- apa_pdf_npages(pdf)
  out <- system2("pdfinfo", c("-f", "1", "-l", as.character(n), shQuote(pdf)),
                 stdout = TRUE)
  as.integer(sub(".*rot:\\s*", "", grep("^Page\\s+[0-9]+\\s+rot:", out,
                                        value = TRUE)))
}

# "portrait" / "landscape" per page, decided by RASTERIZING the page and
# looking at the shape of the image.
#
# This is the only rotation check in this suite that has never been caught
# lying. pdftoppm applies whatever rotation the page carries, by whatever
# mechanism, so a page that comes out wider than it is tall really is a
# landscape page as a reader would see it. 10 dpi is plenty -- nothing here
# reads the pixels, only the width and the height -- and the PNG header
# carries both in bytes 17-24, so no image package is needed.
apa_png_dims <- function(f) {
  h <- readBin(f, "raw", n = 24L)
  c(sum(as.integer(h[17:20]) * 256^(3:0)),
    sum(as.integer(h[21:24]) * 256^(3:0)))
}

apa_pdf_page_shapes <- function(pdf) {
  d <- file.path(tempdir(), paste0("shapes-", basename(tempfile())))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  system2("pdftoppm", c("-r", "10", "-png", shQuote(pdf),
                        shQuote(file.path(d, "p"))),
          stdout = FALSE, stderr = FALSE)
  # pdftoppm zero-pads the page number to the width of the page count, so a
  # plain sort is document order for any document this suite renders.
  fs <- sort(list.files(d, pattern = "[.]png$", full.names = TRUE))
  vapply(fs, function(f) {
    wh <- apa_png_dims(f)
    if (wh[1] > wh[2]) "landscape" else "portrait"
  }, character(1), USE.NAMES = FALSE)
}

apa_pdf_pages <- function(pdf) {
  n <- apa_pdf_npages(pdf)
  vapply(seq_len(n), function(i)
    apa_nbsp_to_space(paste(system2("pdftotext",
                  c("-layout", "-f", i, "-l", i, shQuote(pdf), "-"),
                  stdout = TRUE), collapse = "\n")),
    character(1))
}

# Table numbers in caption order.
#
# (?m) is required: `pages` holds one whole page per element, so without
# multi-line mode `^` anchors to the start of the PAGE and only the first
# caption on each page is ever seen.
apa_pdf_caption_numbers <- function(pages) {
  as.integer(unlist(regmatches(
    pages, gregexpr("(?m)^\\s*Table\\s+\\K[0-9]+", pages, perl = TRUE))))
}

# The numbers the BODY references, read off a marked line.
#
# The document body carries a line "XREF: @tbl-a, @tbl-b, @tbl-c", which
# renders as "XREF: Table 1, Table 2, Table 3". Reading the numbers from that
# one line -- rather than from every "Table N" in the document -- keeps
# captions and cross-references independent, which matters because the
# counter bug corrupts BOTH and a check that conflates them cannot see it.
apa_xref_numbers <- function(pages) {
  lines <- unlist(strsplit(pages, "\n"))
  hit <- grep("XREF:", lines, value = TRUE)
  if (!length(hit)) return(integer(0))
  as.integer(unlist(regmatches(
    hit[1], gregexpr("(?<=Table )[0-9]+", hit[1], perl = TRUE))))
}

# 1-based page number where `marker` first appears, or NA.
#
# Adjacency is the whole point of the note assertion: with the note-placement
# bug every marker still EXISTS somewhere in the document, so only the PAGE
# it lands on distinguishes a correct render from a broken one.
apa_page_of <- function(pages, marker) {
  hit <- which(vapply(pages, function(p) grepl(marker, p, fixed = TRUE),
                      logical(1)))
  if (!length(hit)) NA_integer_ else as.integer(hit[1])
}

# Paragraph styles, in order, from a .docx -- this is how the APA furniture
# (FigureTitle / Caption / FigureNote) is checked without rendering.
apa_docx_styles <- function(docx) {
  d <- tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  utils::unzip(docx, files = "word/document.xml", exdir = d)
  x <- paste(readLines(file.path(d, "word", "document.xml"), warn = FALSE),
             collapse = "")
  unlist(regmatches(x, gregexpr('(?<=<w:pStyle w:val=")[^"]+', x, perl = TRUE)))
}

apa_docx_text <- function(docx) {
  d <- tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  utils::unzip(docx, files = "word/document.xml", exdir = d)
  x <- paste(readLines(file.path(d, "word", "document.xml"), warn = FALSE),
             collapse = "")
  apa_nbsp_to_space(gsub("<[^>]+>", "", gsub("</w:p>", "\n", x)))
}

# ---------------------------------------------------------------------------
# The document under test.
#
# One .qmd per cell of the matrix. FOUR tables in a deliberately mixed order --
# fits / spans pages / fits / fits -- because a document whose tables are all
# the same shape numbers correctly even with the counter bug unpatched, so a
# suite of pure documents would pass against stock apaquarto and prove nothing.
#
# The fourth table is not decoration and the mix is not "any mix". Measured
# against stock apaquarto (and re-confirmed by reverting the patch in a
# rendered project):
#
#   fits fits fits             1 2 3    stock is CORRECT
#   spans spans spans          1 2 3    stock is CORRECT
#   fits spans fits            1 2 3    stock is CORRECT  <- three is not enough
#   spans fits spans           1 2 4    stock is WRONG
#   fits spans fits fits       1 2 3 5  stock is WRONG
#
# The unreset flag latches on the first captioned longtable and then costs the
# SECOND ordinary float after it, not the first -- so a three-table document in
# this order renders correctly with the patch reverted, and a matrix built on
# one would certify nothing. The fourth table is what makes the numbering
# assertion able to fail.
#
# Every marker is PER-TABLE. A single shared marker would let one surviving
# table vouch for two broken ones.
#
#   LASTROW-k     the first cell of table k's final row, so the survival of
#                 each table's bottom row is checkable on its own.
#   NOTE-MARKER-k inside table k's apa-note, so a note can be tied to the page
#                 its own table ends on rather than merely "present somewhere".
#   ENDMARKER     a paragraph of body text AFTER every table, and the only
#                 marker placement is decided against. A marker BEFORE the
#                 tables cannot decide it: under endfloat the first deferred
#                 table lands on the page right after the body, so every
#                 comparison against it is true in both placements. There is
#                 deliberately no such marker here -- one existed, was
#                 measured to be constant, and was removed rather than left
#                 beside its replacement.
#   XREF:         one line of cross-references, read independently of the
#                 captions.
#
# The tables are apa.1way.table() output because its row labels are factor
# LEVELS -- so a marker can be put in the final row verbatim -- and because it
# is three columns wide whatever its height, which lets the middle table be
# made genuinely taller than the page without also being too wide to fit.
apa_acceptance_qmd <- function(placement, orientation, format,
                               self_repair = FALSE) {
  fmt <- if (identical(format, "pdf")) "apaquarto-pdf" else "apaquarto-docx"
  # The .tex (and, where floats are delayed, the .ttt) is kept for the PDF
  # cells so that the MECHANISM can be read as well as the result: which
  # environment the table came out in, and whether the landscape wrapper
  # survived endfloat. What is rotated is decided from the rendered pages,
  # by apa_pdf_page_shapes().
  opts <- c(if (identical(format, "pdf")) "keep-tex: true",
            if (identical(placement, "intext")) "floatsintext: true")
  fmt_block <- if (length(opts)) {
    paste0("format:\n  ", fmt, ":\n",
           paste0("    ", opts, "\n", collapse = ""))
  } else {
    paste0("format: ", fmt, "\n")
  }
  land <- identical(orientation, "landscape")
  ls_open <- if (land) "::: {.landscape}\n\n" else ""
  ls_close <- if (land) ":::\n\n" else ""

  # In the landscape cells EVERY table is rotated, including the one that spans
  # pages, and including it in the placement that collects tables at the end.
  #
  # Do not exclude that combination: it is the one that matters. Rotating with
  # apa7's sidewaystable -- a FLOAT, and a longtable cannot sit in a float --
  # turns a rotated multi-page table at the end upright, starred, 51 of 60
  # rows, with its note missing from the PDF entirely. Rotation is a literal
  # landscape block that endfloat defers whole, and this cell is what covers
  # WIDE and LONG, tables collected at the end.
  tbl <- function(k, cap, n, longtable) {
    # No page argument: what rotates the page here is the ::: {.landscape}
    # div around the chunk. (apa_flextable() has no `page =` argument; Word's
    # orientation comes from the table's own `landscape` field.)
    paste0(
      "```{r}\n",
      "#| label: tbl-t", k, "\n",
      "#| tbl-cap: \"", cap, "\"\n",
      "#| apa-note: \"NOTE-MARKER-", k, " is the note belonging to this table.\"\n",
      # The long table keeps the old chunk option ON PURPOSE -- this matrix is
      # where a document written against the previous release is checked to
      # still render -- but KNOW WHAT THAT COSTS. The branch in
      # apafloatlatex.lua reads `apa-longtable == "true" OR
      # haslongtable(float)`, so this line satisfies the first disjunct and
      # the detection is never reached. MEASURED: with an interim landscape
      # guard (one that blocks the longtable detection) restored by hand,
      # every cell of this matrix stays GREEN, including the landscape/end
      # cell, while the dedicated test below fails with 19 rows missing. So
      # the matrix does NOT cover that regression and must not be counted as
      # covering it; the fixture below, which carries no such option, is the
      # only thing that does. The line stays only because this matrix is the
      # backward-compatibility cell, where a document written against the
      # previous release is checked to still render unchanged.
      if (longtable) "#| apa-longtable: true\n" else "",
      "#| echo: false\n",
      "#| message: false\n",
      "#| warning: true\n",
      "apa.1way.table(iv = g, dv = y, data = acc_data(", n,
      ", \"LASTROW-", k, "\"))\n",
      "```\n\n")
  }

  paste0(
    "---\n",
    "title: \"An Acceptance Matrix Manuscript\"\n",
    "shorttitle: \"Acceptance\"\n",
    "author:\n",
    "  - name: Test Author\n",
    "    corresponding: true\n",
    "    email: test@example.com\n",
    "    affiliations:\n",
    "      - name: Test University\n",
    # A format key with no options under it is a YAML validation ERROR in
    # Quarto ("has empty value but it must instead be an object"), not a
    # harmless empty mapping, so the scalar form is used when there is nothing
    # to say.
    fmt_block,
    "---\n\n",
    "```{r}\n",
    "#| label: acc-setup\n",
    "#| echo: false\n",
    "#| message: false\n",
    "library(apaTables)\n",
    # The shipped template puts this line in every document, so a document
    # rendered on a project someone has run `quarto add` in repairs itself
    # mid-render. What that repair can and cannot reach for the render already
    # under way is what the clobbered cell measures.
    if (isTRUE(self_repair)) "apa.quarto.setup()\n" else "",
    "set.seed(20260731)\n",
    "acc_data <- function(n, marker) {\n",
    "  lv <- c(sprintf(\"Group%02d\", seq_len(n - 1L)), marker)\n",
    "  data.frame(g = factor(rep(lv, each = 4L), levels = lv),\n",
    "             y = stats::rnorm(4L * n, 10, 2))\n",
    "}\n",
    "```\n\n",
    "## Results\n\n",
    "This paragraph opens the body of the manuscript, above every table.\n\n",
    "XREF: @tbl-t1, @tbl-t2, @tbl-t3, @tbl-t4.\n\n",
    ls_open,
    tbl(1, "A Table That Fits on One Page", 5, FALSE),
    tbl(2, "A Table That Spans More Than One Page", 60, TRUE),
    tbl(3, "A Second Table That Fits on One Page", 5, FALSE),
    tbl(4, "A Third Table That Fits on One Page", 5, FALSE),
    ls_close,
    # The TRAILING body paragraph is what makes placement decidable. Comparing
    # tables against a marker BEFORE them cannot fail: under endfloat the first
    # deferred table lands on the page immediately after the body, so
    # "some table is at most one page after the body" is true of a document
    # with no tables in the text at all. Against a marker that comes AFTER the
    # tables in the source, the two placements are opposites -- with floats in
    # text some table precedes it, with floats at the end every table follows
    # it -- and each direction is asserted, so stripping floatsintext turns the
    # cell red instead of leaving it green.
    "ENDMARKER closes the body of this manuscript, after every table has been ",
    "declared.\n")
}

# A one-table document holding the hardest combination: a table that
# continues onto the next page, inside a ::: {.landscape} div, in a
# manuscript that collects its tables at the end.
#
# The two things it asks for are incompatible if rotation is apa7's
# sidewaystable, which is a float, because a longtable cannot live in a float:
# the table comes out upright, starred, 51 of 60 rows, with its note missing
# from the PDF entirely and not a word said. Rotation is a literal landscape
# block that endfloat defers whole, so it needs no float and this document is
# simply correct.
#
# THE CHUNK CARRIES NO `apa-longtable` OPTION, AND THAT ABSENCE IS THE WHOLE
# TEST. With one, the fixture cannot observe the defect it exists for: the
# branch in apafloatlatex.lua reads
#
#   apa-longtable == "true"  OR  haslongtable(float)
#
# so the option satisfies the FIRST disjunct and the second -- the detection
# itself -- is never reached. Measured: with an interim guard in front of the
# detection restored by hand AND the option present, this document renders
# 60 of 60 rows, unstarred, no float, rotated -- identical to the passing
# case, so the suite stays green against the exact regression it exists to
# catch. With the option gone and the guard restored it drops to 22 of 60,
# note absent from the PDF entirely, and the test fails loudly.
#
# A fixture must never hand the code under test the answer it is supposed to
# work out.
#
# The table is delivered through the native LaTeX emitter, the only path that
# can put a table on a rotated page. No warning asks for the missing line:
# the render is correct without it.
apa_landscape_longtable_qmd <- function() {
  paste0(
    "---\n",
    "title: \"A Rotated Table That Spans Pages\"\n",
    "shorttitle: \"Rotated Long\"\n",
    "author:\n",
    "  - name: Test Author\n",
    "    corresponding: true\n",
    "    email: test@example.com\n",
    "    affiliations:\n",
    "      - name: Test University\n",
    "format:\n  apaquarto-pdf:\n    keep-tex: true\n",
    "---\n\n",
    "```{r}\n",
    "#| label: acc-setup\n",
    "#| echo: false\n",
    "#| message: false\n",
    "library(apaTables)\n",
    "set.seed(20260731)\n",
    "lv <- c(sprintf(\"Group%02d\", seq_len(59L)), \"LASTROW-1\")\n",
    "acc <- data.frame(g = factor(rep(lv, each = 4L), levels = lv),\n",
    "                  y = stats::rnorm(240L, 10, 2))\n",
    "```\n\n",
    "## Results\n\n",
    "This paragraph is the body of the manuscript.\n\n",
    "::: {.landscape}\n\n",
    "```{r}\n",
    "#| label: tbl-t1\n",
    "#| tbl-cap: \"A Table That Spans More Than One Page\"\n",
    "#| apa-note: \"NOTE-MARKER-1 is the note belonging to this table.\"\n",
    "#| echo: false\n",
    "#| message: false\n",
    # warning: true, deliberately. A regression here could come with a
    # warning typed into the manuscript, and suppressing it would hide that
    # from the very test that exists to catch one.
    "#| warning: true\n",
    "knitr::asis_output(apaTables:::apa_latex_table(\n",
    "  apaTables:::apa_display_plan(\n",
    "    apa.1way.table(iv = g, dv = y, data = acc)), 10))\n",
    "```\n\n",
    ":::\n")
}

# Turn a project's copy of the extension back into a stock-like one -- what a
# user gets by running `quarto add wjschne/apaquarto`, which reverts our copy
# in place while printing "[No Change]".
#
# Only the four patches are undone, and each in the way that matters to the
# render rather than cosmetically:
#
#   * the two Lua filters apaTables adds are deleted AND unhooked from the
#     filter chain in _extension.yml, because the chain is what pandoc reads;
#   * the longtable counter reset in apatemplate.tex is put back to upstream's
#     (the version that reports NOT-FOUND and never fires);
#   * the version loses its "+apaTables.N" suffix, so apa.quarto.setup() sees a
#     copy that is not ours and repairs it exactly as it would in the field.
apa_unpatch_extension <- function(dir) {
  for (f in c("apalongtablenote.lua", "apalandscapelatex.lua"))
    unlink(file.path(dir, f))

  yml <- file.path(dir, "_extension.yml")
  x <- readLines(yml, warn = FALSE)
  # Drop each "path: <our filter>" line together with the "- at: <stage>" line
  # immediately above it; a stranded "- at:" would be an invalid chain entry.
  drop <- grep("path: (apalongtablenote|apalandscapelatex)[.]lua", x)
  x <- x[-sort(unique(c(drop, drop - 1L)))]
  x <- sub("^(version: .*)[+]apaTables[.][0-9]+\\s*$", "\\1", x)
  writeLines(x, yml)

  tpl <- file.path(dir, "apatemplate.tex")
  t <- readLines(tpl, warn = FALSE)
  i <- grep("^\\\\newif\\\\ifLTpatch@caption$", t)
  j <- grep("^\\\\makeatother$", t)
  j <- j[j > i][1]
  stopifnot(length(i) == 1L, !is.na(j))
  writeLines(c(t[seq_len(i - 1L)],
    "\\patchcmd{\\LT@caption}", "  {\\bgroup}",
    "  {\\bgroup\\global\\LTpatch@captiontrue}", "  {}{}",
    "\\patchcmd{\\longtable}", "  {\\par}",
    "  {\\par\\global\\LTpatch@captionfalse}", "  {}{}",
    "\\apptocmd{\\endlongtable}",
    "  {\\ifLTpatch@caption\\else\\addtocounter{table}{-1}\\fi}", "  {}{}",
    "\\newif\\ifLTpatch@caption", "\\makeatother",
    t[(j + 1L):length(t)]), tpl)
  invisible(dir)
}

# ---------------------------------------------------------------------------
# Structural .docx helpers, built for tests/testthat/test-docx-structure.R
# and reused elsewhere.
#
# Everything here reads values back out of a REAL rendered .docx, the same
# discipline the PDF helpers above follow -- a claim about a section break or
# a header reference is only worth as much as the XML it can be read off.

# Render one fixture under tests/testthat/fixtures/<name>.qmd to .docx
# through a REAL `quarto render`, in a scratch apaTables Quarto project (so
# the CURRENT bundled apaquarto extension -- this source tree under
# pkgload::load_all(), or the installed package otherwise -- travels with
# it, never whatever a stale project directory happened to have lying
# around). Returns the path to the rendered .docx.
#
# Errors LOUDLY, with the render's own stdout/stderr attached, rather than
# returning a path to a file that was never written -- this is a shared
# helper called from inside other assertions, and a silent NULL/missing-file
# return here would surface many calls later as a confusing unzip failure
# instead of as what actually happened: the render did not produce output.
apa_render_fixture_docx <- function(name) {
  src <- testthat::test_path("fixtures", paste0(name, ".qmd"))
  if (!file.exists(src)) {
    stop("No such fixture: ", src, call. = FALSE)
  }
  proj <- file.path(tempdir(), paste0("docx-fixture-", name, "-",
                                      as.integer(Sys.time())))
  unlink(proj, recursive = TRUE)
  suppressMessages(apa.quarto.new(proj, filename = "doc.qmd"))
  file.copy(src, file.path(proj, "doc.qmd"), overwrite = TRUE)

  log <- suppressWarnings(system2(
    "quarto", c("render", shQuote(file.path(proj, "doc.qmd"))),
    stdout = TRUE, stderr = TRUE))
  out <- file.path(proj, "doc.docx")
  if (!file.exists(out)) {
    stop("quarto render produced no doc.docx for fixture '", name, "':\n",
        paste(log, collapse = "\n"), call. = FALSE)
  }
  out
}

# The HTML sibling of apa_render_fixture_docx() above -- same scratch-project
# discipline (the CURRENT bundled apaquarto extension travels with the
# render, never a stale project directory's copy), same loud failure on a
# missing output file. HTML needs a REAL rendered fixture: assertions that
# only mock apa_latex_target() cannot see a docx-only shrink-ladder
# consequence (the `apa-table-size-N` marker div, and its floor warning)
# leaking into HTML output.
apa_render_fixture_html <- function(name) {
  src <- testthat::test_path("fixtures", paste0(name, ".qmd"))
  if (!file.exists(src)) {
    stop("No such fixture: ", src, call. = FALSE)
  }
  proj <- file.path(tempdir(), paste0("html-fixture-", name, "-",
                                      as.integer(Sys.time())))
  unlink(proj, recursive = TRUE)
  suppressMessages(apa.quarto.new(proj, filename = "doc.qmd"))
  file.copy(src, file.path(proj, "doc.qmd"), overwrite = TRUE)

  log <- suppressWarnings(system2(
    "quarto", c("render", shQuote(file.path(proj, "doc.qmd"))),
    stdout = TRUE, stderr = TRUE))
  out <- file.path(proj, "doc.html")
  if (!file.exists(out)) {
    stop("quarto render produced no doc.html for fixture '", name, "':\n",
        paste(log, collapse = "\n"), call. = FALSE)
  }
  out
}

# The raw contents of a rendered .html, as one string -- HTML has no zip
# container to unpack, so this is just a read, kept as a named helper for
# the same reason apa_docx_document_xml() is: one read per fixture, not one
# per assertion.
apa_html_text <- function(html_path) {
  paste(readLines(html_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# The raw word/document.xml of a rendered .docx, as one string -- the base
# every other docx-structure helper below reads from, so a fixture is only
# unzipped once per call site rather than once per helper.
apa_docx_document_xml <- function(docx) {
  d <- tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  utils::unzip(docx, files = "word/document.xml", exdir = d)
  paste(readLines(file.path(d, "word", "document.xml"), warn = FALSE),
        collapse = "")
}

# Every <w:sectPr>...</w:sectPr> block in a document.xml string, in
# document order -- both the ones wrapped in a closing paragraph's <w:pPr>
# (a section break mid-document) and the final, unwrapped one at the very
# end of <w:body> (the document's default/trailing section). One vector
# entry per block; apa_docx_sections() does no interpretation of what is
# inside them, which is deliberately left to the caller (the section
# orientation tests read w:orient and w:pgSz off each one themselves).
#
# The pattern has TWO alternatives on purpose. `<w:sectPr\b.*?</w:sectPr>`
# alone cannot see a genuinely SELF-CLOSING `<w:sectPr/>` (no attributes, no
# children -- the shape Quarto's own `.landscape` div closer can leave
# behind) -- that block would simply be dropped from the returned vector,
# silently SHORTENING it, the worst failure mode for an all-items loop (a
# caller iterating "every section" would just never see the missing one,
# rather than getting an error). No fixture in this suite produces one
# (apatablelandscape.lua replaces that mechanism), so this is proven by a
# synthetic string, not a render: `<w:sectPr/>` in, and the single-
# alternative pattern is demonstrably blind to it (see the "self-closing"
# test).
apa_docx_sections <- function(doc_xml) {
  m <- regmatches(doc_xml,
                  gregexpr("<w:sectPr\\b[^>]*/>|<w:sectPr\\b.*?</w:sectPr>",
                           doc_xml, perl = TRUE))[[1]]
  if (!length(m)) character(0) else m
}

# Every <w:tblGrid>...</w:tblGrid> block's own <w:gridCol w:w="..."> widths
# (twips), one integer vector per table, in document order -- for the docx
# column-geometry checks. Unlike apa_docx_table_styles() below, a
# <w:tblGrid> is self-contained (its own open/close tag, never nested inside
# another table's), so there is no cross-table boundary for a lazy scan to
# run past and no bounding is needed here.
apa_docx_tbl_grids <- function(doc_xml) {
  m <- regmatches(doc_xml,
                  gregexpr("<w:tblGrid>.*?</w:tblGrid>", doc_xml, perl = TRUE))[[1]]
  lapply(m, function(g) {
    as.integer(regmatches(g, gregexpr('(?<=w:w=")[0-9]+', g, perl = TRUE))[[1]])
  })
}

# One integer attribute off an XML tag/fragment, found by NAME rather than
# position -- pandoc's own trailing sectPr and officer's per-table one order
# their pgSz/pgMar attributes differently, so a fixed-position parse would be
# writer-specific. NA when the attribute is not present, so a caller
# comparing pgSz's bare `w:w=` against pgMar's `w:left=`/`w:right=` (all on
# the same combined sectPr string) never collides -- each name is searched
# for literally, and pgMar has no bare `w:w=` attribute to be confused with
# pgSz's.
apa_docx_xml_int_attr <- function(tag, name) {
  m <- regmatches(tag, regexpr(paste0(name, '="([0-9]+)"'), tag, perl = TRUE))
  if (!length(m) || !nzchar(m)) return(NA_integer_)
  as.integer(sub(paste0(name, '="([0-9]+)"'), "\\1", m, perl = TRUE))
}

# The `custom-style` a table ended up with, one entry per `<w:tbl>` in
# document order -- built for the multi-table regression check:
# apatablesize.lua must pair each size marker with only the table it belongs
# to, never share ONE size across every table in the document, and this is
# how the pairing is checked from the rendered .docx rather than trusted
# from the filter's own logic. Every table pandoc's docx
# writer produces from the bundled reference document carries an explicit
# `<w:tblStyle w:val="...">` inside its own `<w:tblPr>`, immediately after
# `<w:tbl>` and before any row content, so bounding the search to that
# short span (rather than a bare non-greedy scan to the next occurrence
# anywhere in the document) is what keeps this correct even when SOME
# tables in the document have no marker at all.
#
# Do not replace this with a single lazy scan across the WHOLE document
# (`<w:tbl>\\s*<w:tblPr>.*?<w:tblStyle w:val="([^"]+)"`): with no stop at the
# next `<w:tbl`, a table genuinely lacking a `<w:tblStyle>` lets the search
# run forward and pick up a LATER table's style -- silent misattribution.
# So it works one table at a time: split the document into its own
# `<w:tbl>...</w:tbl>` blocks first (tables are never nested in anything
# this codebase produces -- apa_docx_tbl_grids()'s own comment makes the same
# assumption), then search each block in isolation. A table with no
# `<w:tblStyle>` reports NA_character_ at ITS OWN position instead of
# silently vanishing and letting the next table's style shift into its slot.
apa_docx_table_styles <- function(doc_xml) {
  tbls <- regmatches(doc_xml, gregexpr(
    '<w:tbl>.*?</w:tbl>', doc_xml, perl = TRUE))[[1]]
  vapply(tbls, function(t) {
    m <- regmatches(t, regexpr('<w:tblStyle w:val="([^"]+)"', t, perl = TRUE))
    if (!length(m) || !nzchar(m)) return(NA_character_)
    sub('<w:tblStyle w:val="([^"]+)".*', "\\1", m)
  }, character(1), USE.NAMES = FALSE)
}

# The HEADER relationship IDs a rendered .docx's word/_rels/document.xml.rels
# actually carries, as a named character vector (id -> target file), sorted
# by increasing numeric ID -- the order measured stable (rId8/rId9/rId12)
# and that apatablelandscape.lua hardcodes. Footer
# relationships are deliberately excluded: the one caller that exists
# (test-docx-structure.R) checks this against apaTables' own hardcoded
# header IDs, and including footers here would make that comparison silently
# permissive to a footer-only drift the filter does not depend on.
apa_docx_header_rels <- function(docx) {
  d <- tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  utils::unzip(docx, files = "word/_rels/document.xml.rels", exdir = d)
  relsfile <- file.path(d, "word", "_rels", "document.xml.rels")
  if (!file.exists(relsfile)) return(character(0))
  x <- paste(readLines(relsfile, warn = FALSE), collapse = "")
  entries <- regmatches(x, gregexpr(
    '<Relationship\\b[^>]*Type="[^"]*/header"[^>]*/>', x, perl = TRUE))[[1]]
  if (!length(entries)) return(character(0))
  ids <- sub('.*\\bId="([^"]+)".*', "\\1", entries)
  targets <- sub('.*\\bTarget="([^"]+)".*', "\\1", entries)
  out <- stats::setNames(targets, ids)
  out[order(as.integer(sub("^rId", "", names(out))))]
}

# Both header AND footer relationships, same shape as apa_docx_header_rels()
# above (id -> target file, sorted by numeric id).
#
# apa_docx_header_rels() deliberately excludes footers, and its own comment
# says why: its one caller compares them against apaTables' hardcoded HEADER
# ids, and folding footers into that vector would make the comparison
# silently permissive. But apatablelandscape.lua hardcodes SIX references,
# not three -- rId10/rId11/rId13 are footers. This is the vector that lets
# those be checked against a rendered document, without weakening the
# header-only comparison.
apa_docx_hdrftr_rels <- function(docx) {
  d <- tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  utils::unzip(docx, files = "word/_rels/document.xml.rels", exdir = d)
  relsfile <- file.path(d, "word", "_rels", "document.xml.rels")
  if (!file.exists(relsfile)) return(character(0))
  x <- paste(readLines(relsfile, warn = FALSE), collapse = "")
  entries <- regmatches(x, gregexpr(
    '<Relationship\\b[^>]*Type="[^"]*/(header|footer)"[^>]*/>', x,
    perl = TRUE))[[1]]
  if (!length(entries)) return(character(0))
  ids <- sub('.*\\bId="([^"]+)".*', "\\1", entries)
  targets <- sub('.*\\bTarget="([^"]+)".*', "\\1", entries)
  out <- stats::setNames(targets, ids)
  out[order(as.integer(sub("^rId", "", names(out))))]
}

# ---------------------------------------------------------------------------
# Where apafloatstoend.lua puts its page breaks.
#
# The runs of content BETWEEN the explicit page breaks apafloatstoend.lua
# emits, in document order, as plain text.
#
# This is the only shape in which two end-float defects are observable from
# the file: a trailing blank page, and Table 1 printing under the reference
# list. A trailing EMPTY segment IS the blank page a reader sees, and
# "which segment is Table 1 in" is how you ask whether it shares a page with
# the reference list. Nothing that counts words or tables can see either: the
# blank page carries a running head and a page number, so a text extractor does
# not call it empty -- only LOOKING at a rendered page, or this, shows it.
#
# NOT a page model, and no assertion here may pretend otherwise. Word breaks
# pages by itself as content overflows; these are only the breaks the FILTER
# asked for, so one segment can span several real pages. Every assertion built
# on this must be about an EXPLICIT break.
#
# The sentinel is load-bearing. R's strsplit() DROPS a trailing empty field --
# measured here: strsplit("a-b-", "-", fixed = TRUE)[[1]] returns c("a", "b"),
# not c("a", "b", "") -- so the blank page, which is precisely a trailing
# empty field, would be INVISIBLE to the one assertion that exists to see it.
# Appending a sentinel guarantees a final non-empty field, which is then
# trimmed back off.
apa_docx_page_segments <- function(doc_xml) {
  brk <- '<w:p><w:r><w:br w:type="page"/></w:r></w:p>'
  sentinel <- "ZZENDOFBODYZZ"
  parts <- strsplit(paste0(doc_xml, sentinel), brk, fixed = TRUE)[[1]]
  n <- length(parts)
  parts[n] <- substr(parts[n], 1L, nchar(parts[n]) - nchar(sentinel))
  txt <- gsub("<[^>]+>", " ", parts)
  txt <- apa_nbsp_to_space(txt)
  trimws(gsub("[[:space:]]+", " ", txt))
}
