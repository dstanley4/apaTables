# Landscape helpers. apa_docx_document_xml() already exists in
# helper-apaquarto-extract.R and is reused directly -- this file adds only
# what does not exist yet.

# A CONSTRUCTED table whose flextable width lands strictly between
# apa_usable_width(FALSE, "a4") (7.27 in) and apa_usable_width(FALSE, "us")
# (7.50 in) -- the only place a hardcoded portrait threshold of 7.5 can be
# caught. No real apaTables example lands there (the eight builders'
# shipped examples measure 2.16, 4.56, 5.21, 6.18, 8.57, 8.61, 10.23 and
# 12.76 in), so the width is manufactured by widening one factor level's
# name in a plain apa.1way.table() until flextable's own measurement of the
# widened column lands in the gap.
#
# CALIBRATED AT RUNTIME, NOT HARDCODED TO A SINGLE PADDING LENGTH. The
# measured width comes from systemfonts' metrics for Times New Roman
# (R/render-flextable.R), and the band is only 0.23 in wide -- on a machine
# that lacks that font, systemfonts substitutes something else and the width
# for any FIXED padding length can jump by far more than 0.23 in, which would
# fail R CMD check on a CRAN machine we cannot see (not skip_on_cran()'s
# job to hide, since the two orientation assertions this table feeds, the
# "rotation threshold comes from paper" tests, are portable). So this
# searches upward for a padding length that lands in the band on WHATEVER
# font this machine actually measures with, and skips -- rather than
# failing -- if none in a bounded range does; a font-metric mismatch this
# large reads as "cannot construct the case here", not a red CRAN check.
#
# The band membership itself is additionally asserted by the caller (belt and
# suspenders): this helper's own search already enforces it before
# returning, but the test-level assertion is what keeps that guarantee
# visible to a reader who never opens this file.
band_table_7_27_to_7_50 <- function() {
     lo <- apaTables:::apa_usable_width(FALSE, "a4")   # 7.27
     hi <- apaTables:::apa_usable_width(FALSE, "us")   # 7.50
     for (n in 1:300) {
          d <- viagra
          levels(d$dose) <- c(strrep("A", n), "Low Dose", "High Dose")
          tb <- apa.1way.table(iv = dose, dv = libido, data = d)
          w  <- flextable::flextable_dim(apaTables:::apa_flextable(tb))$widths
          if (w > lo && w < hi) return(tb)
     }
     testthat::skip(sprintf(
          "no padding length in 1:300 produced a flextable width strictly between %.4f and %.4f in on this machine's font metrics -- cannot construct the A4/US-Letter band case here",
          lo, hi))
}

# ---------------------------------------------------------------------------
# Drive apatablelandscape.lua over a div with CHOSEN page geometry and read
# the section properties it writes back out of a real .docx.
#
# Why this exists rather than another apa_render_fixture_docx() call: the
# bundled reference document's margins are SYMMETRIC (1440 twips on every
# side), so rotating the margins with the page is a no-op for every fixture
# in this suite and cannot be observed from one. The margins have to be
# chosen, and the only
# place they are chosen is the div's own attributes -- which is exactly what
# apa_landscape_wrap() writes and what this feeds in directly.
#
# Bare `pandoc`, not `quarto render`: the filter is a plain pandoc Lua
# filter and the two closers it writes are RawBlocks that pandoc's own docx
# writer passes straight through (verified -- both closers appear in
# word/document.xml with no reference document involved). That keeps this a
# sub-second unit test of the FILTER rather than a tens-of-seconds render of
# the whole toolchain. It reads the INSTALLED filter via system.file(),
# which under pkgload::load_all()/devtools::test() resolves to this source
# tree's own inst/ copy -- the same convention test-tablemeta-decision.R
# uses, so this tests the branch's filter and not a stale library's.
#
# Returns the <w:sectPr> blocks the FILTER wrote, in document order, in
# their raw XML. Pandoc's own default reference document contributes a
# further trailing sectPr carrying only <w:footnotePr>; it is dropped here
# by requiring a <w:pgMar>, so a caller never has to know it exists.
# `numbered_lines = TRUE` puts `numbered-lines: true` in a YAML header, which
# is what the filter's own Meta pass reads (the same key and stringify check
# docxlinenumber.lua uses) before it decides whether to carry
# `<w:lnNumType>`. Without it the element is simply absent from the output,
# which is why the ordering assertion that actually matters -- lnNumType
# BEFORE cols, the only pair CT_SectPr's sequence can be violated on here --
# could not be written against the default probe.
apa_landscape_filter_sections <- function(mt, mr, mb, ml,
                                          pgw = 12240, pgh = 15840,
                                          mh = 720, mf = 720, mg = 0,
                                          numbered_lines = FALSE) {
  testthat::skip_if(Sys.which("pandoc") == "", "pandoc not available")
  lua <- system.file("_extensions/wjschne/apaquarto/apatablelandscape.lua",
                     package = "apaTables")
  testthat::skip_if(lua == "", "extension not installed")

  d <- tempfile()
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  src <- file.path(d, "in.md")
  out <- file.path(d, "out.docx")
  writeLines(c(
    if (isTRUE(numbered_lines)) c("---", "numbered-lines: true", "---", ""),
    sprintf(paste0('::: {.apa-table-landscape pgw="%s" pgh="%s" mt="%s" ',
                   'mb="%s" ml="%s" mr="%s" mh="%s" mf="%s" mg="%s"}'),
            pgw, pgh, mt, mb, ml, mr, mh, mf, mg),
    "", "| a | b |", "|---|---|", "| 1 | 2 |", "", ":::"), src)

  log <- suppressWarnings(system2(
    "pandoc", c(shQuote(src), "-o", shQuote(out),
                paste0("--lua-filter=", shQuote(lua))),
    stdout = TRUE, stderr = TRUE))
  if (!file.exists(out)) {
    stop("pandoc wrote no .docx for the landscape filter probe:\n",
         paste(log, collapse = "\n"), call. = FALSE)
  }
  secs <- apa_docx_sections(apa_docx_document_xml(out))
  secs[grepl("<w:pgMar", secs, fixed = TRUE)]
}

# A minimal synthetic .docx whose trailing <w:sectPr> carries CHOSEN page
# geometry -- for the two tests that have to tell the project's copy of
# apaquarto.docx apart from the installed package's.
#
# The bundled reference document is US Letter with symmetric 1440 margins,
# so a test that fed it to both branches could not tell which one answered.
# This writes a file that is nothing else: a4 = TRUE is not a claim about
# apaquarto (there is no A4 docx reference document, see
# test-docx-structure.R's own note), only a set of numbers no bundled file
# has.
apa_write_synthetic_docx <- function(path, w = 11906, h = 16838,
                                     top = 1111, right = 2222,
                                     bottom = 3333, left = 4444,
                                     header = 555, footer = 666, gutter = 0) {
  d <- tempfile()
  dir.create(file.path(d, "word"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  writeLines(paste0(
    '<?xml version="1.0"?><w:document ',
    'xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">',
    "<w:body><w:p><w:r><w:t>x</w:t></w:r></w:p>",
    sprintf('<w:sectPr><w:pgSz w:w="%s" w:h="%s"/>', w, h),
    sprintf(paste0('<w:pgMar w:top="%s" w:right="%s" w:bottom="%s" ',
                   'w:left="%s" w:header="%s" w:footer="%s" w:gutter="%s"/>'),
            top, right, bottom, left, header, footer, gutter),
    "</w:sectPr></w:body></w:document>"),
    file.path(d, "word", "document.xml"))
  path <- normalizePath(path, mustWork = FALSE)
  old <- setwd(d)
  # after = FALSE, so this runs BEFORE the unlink() registered above.
  # on.exit(add = TRUE) APPENDS, so the default order
  # would remove `d` while the process working directory is still inside it
  # -- tolerated on macOS/Linux, an error on Windows, where a directory in
  # use cannot be deleted.
  on.exit(setwd(old), add = TRUE, after = FALSE)
  utils::zip(path, "word/document.xml", flags = "-q")
  invisible(path)
}
