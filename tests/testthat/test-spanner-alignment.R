# D119: a column spanner is CENTRED over the columns it covers.
#
# THE DEFECT, measured in a real render before the fix: `alcohol` printed hard
# against the RIGHT edge of the three columns it spans, in BOTH docx and html.
# Pandoc gives a spanning cell the alignment of the FIRST column it covers,
# and over numeric columns that is right. The PDF path was already correct and
# is untouched -- apaTables emits `\multicolumn{n}{c}` there and never builds
# an AST table for pandoc to align.
#
# The fix is `apatablespanner.lua` (bundled), which sets the alignment on the
# AST at post-quarto, so ONE rule corrects both writers.
#
# WHAT THESE TESTS ARE REALLY GUARDING is the second half of that filter: it
# touches HEAD rows only. A `col_span > 1` cell in the BODY is a row-group
# label ("Model 1"), which is flush left and must stay flush left. A test that
# only checked the spanner would pass for a filter that centred every spanning
# cell -- and that filter would silently restyle every grouped table in the
# package. The fixture carries both shapes for exactly this reason.
#
# Real renders, and the assertions read the OUTPUT back, never the source.

skip_on_cran()
skip_if_not(nzchar(Sys.which("quarto")), "quarto not installed")

# The spawned quarto R session runs library(apaTables) against the INSTALLED
# package, not this source tree -- see helper-stale-library.R. A green test
# rendered against a stale library certifies the library, not the branch.
apa_skip_if_library_stale()

# Every cell that spans more than one column, in document order, as
# list(span, header, align, text). `header` is TRUE when the cell's ROW is a
# repeating header row (`<w:tblHeader/>`), which is how the head rows are
# identified without assuming how many there are.
apa_docx_spanning_cells <- function(doc) {
  rows <- regmatches(doc, gregexpr("<w:tr>.*?</w:tr>", doc))[[1]]
  out <- list()
  for (r in rows) {
    is_head <- grepl("<w:tblHeader", r, fixed = TRUE)
    cells <- regmatches(r, gregexpr("<w:tc>.*?</w:tc>", r))[[1]]
    for (cl in cells) {
      m <- regmatches(cl, regexpr('w:gridSpan w:val="[0-9]+"', cl))
      if (!length(m)) next
      span <- as.integer(sub('.*"([0-9]+)".*', "\\1", m))
      jc <- regmatches(cl, regexpr('<w:jc w:val="[a-z]+"', cl))
      txt <- paste(regmatches(cl, gregexpr("<w:t[^>]*>[^<]*</w:t>", cl))[[1]],
                   collapse = "")
      out[[length(out) + 1L]] <- list(
        span = span, header = is_head,
        align = if (length(jc)) sub('.*"([a-z]+)".*', "\\1", jc) else NA_character_,
        text = gsub("<[^>]*>", "", txt))
    }
  }
  out
}

test_that("in Word a head spanner is centred and a body row-group label is not", {
  d <- apa_render_fixture_docx("spanner-alignment")
  doc <- apa_docx_document_xml(d)
  cells <- apa_docx_spanning_cells(doc)

  head_cells <- Filter(function(c) c$header, cells)
  body_cells <- Filter(function(c) !c$header, cells)

  # Neither list may be empty, or this test certifies nothing -- the shape of
  # failure that has recurred in this project more than any other.
  expect_gt(length(head_cells), 0L)
  expect_gt(length(body_cells), 0L)

  # THE FIX: every spanning HEAD cell is centred. `alcohol` and the three
  # level labels are four such cells.
  expect_equal(unique(vapply(head_cells, function(c) c$align, character(1))),
               "center")
  expect_true(any(vapply(head_cells, function(c) c$text, character(1)) == "alcohol"))

  # THE GUARD: the row-group labels span every column and are NOT centred.
  # This is what fails if the filter stops restricting itself to head rows.
  expect_false(any(vapply(body_cells, function(c) identical(c$align, "center"),
                          logical(1))))
  expect_true(any(grepl("^Model ", vapply(body_cells, function(c) c$text,
                                          character(1)))))
})

test_that("in HTML a head spanner is centred and a body row-group label is not", {
  # A SEPARATE fixture, whose YAML `format:` and entry-point CALLS agree
  # (D112a). Rendering the docx fixture to HTML would both fail -- the helper
  # looks for doc.html -- and, if forced, would put a guidance warning in the
  # document that no real user would see.
  h <- apa_html_text(apa_render_fixture_html("spanner-alignment-html"))

  # The spanning cells: <th colspan="6" style="text-align: ...">. Read the
  # alignment off the SAME element the browser does, rather than trusting that
  # a stylesheet rule elsewhere would have won -- pandoc writes this one
  # inline, and an inline style beats any selector.
  th <- regmatches(h, gregexpr('<th colspan="[0-9]+"[^>]*>', h))[[1]]
  expect_gt(length(th), 0L)
  expect_true(all(grepl("text-align: center", th, fixed = TRUE)))
  expect_false(any(grepl("text-align: right", th, fixed = TRUE)))

  # And the row-group labels, which are <td colspan=...> in the body, keep
  # their left alignment.
  td <- regmatches(h, gregexpr('<td colspan="[0-9]+"[^>]*>', h))[[1]]
  expect_gt(length(td), 0L)
  expect_false(any(grepl("text-align: center", td, fixed = TRUE)))
})
