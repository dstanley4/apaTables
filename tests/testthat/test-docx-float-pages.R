# Where apafloatstoend.lua puts its page breaks.
#
# THE DEFECT THESE TESTS GUARD AGAINST. A filter that walks the document
# backwards and inserts each float's page break BEFORE prepending the float
# appends the block
#
#     [Table 1] [break] [Table 2] [break] ... [Table n] [break]
#
# -- the break TRAILS each float instead of LEADING it. Two defects follow
# from that one placement: the last break has nothing after it (a blank page
# at the end of the manuscript), and nothing pushes the FIRST float onto a
# page of its own, so Table 1 prints under the reference list. The break
# leads each float and the block is
#
#     [break] [Table 1] [break] [Table 2] ... [break] [Table n]
#
# Every assertion here reads the rendered .docx back. None reads the source
# .qmd: a claim about page placement is worth what the XML it comes off is
# worth.
#
# WHAT THESE TESTS CANNOT SEE, and it is not a small gap. apa_docx_page_segments()
# splits on the page-break paragraphs THIS FILTER emits. A Word section break
# also starts a page, and rotated (landscape) floats are wrapped in one by
# apatablelandscape.lua -- so a blank page produced that way is invisible here.
# The rotated case is evidenced only by rendered documents that were exported
# from Word and looked at.

skip_on_cran()
skip_if_not(nzchar(Sys.which("quarto")), "quarto not installed")

# The spawned quarto R session runs library(apaTables) against the INSTALLED
# package, not this source tree -- see helper-stale-library.R. A green test
# rendered against a stale library certifies the library, not the branch.
apa_skip_if_library_stale()

test_that("the last float leaves no blank page behind", {
  segs <- apa_docx_page_segments(
    apa_docx_document_xml(apa_render_fixture_docx("float-pages-two-tables")))
  expect_gt(length(segs), 1L)
  expect_true(all(nzchar(segs)))
})

test_that("the first table starts a page of its own", {
  segs <- apa_docx_page_segments(
    apa_docx_document_xml(apa_render_fixture_docx("float-pages-two-tables")))

  body <- grep("UNIQUEBODYMARKER", segs, fixed = TRUE)
  one  <- grep("UNIQUETABLEONE",   segs, fixed = TRUE)
  two  <- grep("UNIQUETABLETWO",   segs, fixed = TRUE)
  expect_length(body, 1L); expect_length(one, 1L); expect_length(two, 1L)

  # Both directions, because either alone is satisfiable by an accident.
  expect_false(identical(body, one))
  expect_false(grepl("UNIQUETABLEONE", segs[body], fixed = TRUE))
  expect_false(grepl("UNIQUEBODYMARKER", segs[one], fixed = TRUE))
  # ... and the two tables do not share a page either.
  expect_false(identical(one, two))
})

test_that("a figure after a table gets its own page, and leaves no blank page", {
  segs <- apa_docx_page_segments(
    apa_docx_document_xml(
      apa_render_fixture_docx("float-pages-table-and-figure")))
  expect_true(all(nzchar(segs)))
  tbl <- grep("UNIQUETABLEONE",  segs, fixed = TRUE)
  fig <- grep("UNIQUEFIGUREONE", segs, fixed = TRUE)
  expect_length(tbl, 1L); expect_length(fig, 1L)
  expect_false(identical(tbl, fig))
})

test_that("floatsintext keeps the floats in the body and adds no float breaks", {
  segs <- apa_docx_page_segments(
    apa_docx_document_xml(apa_render_fixture_docx("float-pages-floatsintext")))
  body <- grep("UNIQUEBODYMARKER", segs, fixed = TRUE)
  expect_length(body, 1L)
  # Both tables sit in the SAME segment as the body: nothing was moved and no
  # break was emitted between them.
  expect_true(grepl("UNIQUETABLEONE", segs[body], fixed = TRUE))
  expect_true(grepl("UNIQUETABLETWO", segs[body], fixed = TRUE))
  expect_true(all(nzchar(segs)))
})

# The appendix path. The filter puts a break before the appendix heading, and
# the floats are inserted next to it, so a naive leading break per float would
# DOUBLE that break. The filter steps the insertion point back past the
# appendix break so it is not doubled. These guards pass against both the
# trailing-break and the leading-break filter; they exist so the leading-break
# placement cannot quietly break the appendix case. `all(nzchar(segs))` is what
# observes
# the doubling: a doubled break opens a blank page in the MIDDLE of the
# document, which an assertion looking only at the last segment cannot see.

test_that("an appendix document has no blank page and no float shares a page", {
  segs <- apa_docx_page_segments(
    apa_docx_document_xml(apa_render_fixture_docx("float-pages-appendix")))
  expect_true(all(nzchar(segs)))
  expect_true(grepl("UNIQUEAPPENDIXBODY", segs[length(segs)], fixed = TRUE))
  one <- grep("UNIQUETABLEONE", segs, fixed = TRUE)
  two <- grep("UNIQUETABLETWO", segs, fixed = TRUE)
  expect_length(one, 1L); expect_length(two, 1L)
  expect_false(identical(one, two))
  expect_false(grepl("UNIQUEBODYMARKER", segs[one], fixed = TRUE))
})

test_that("two appendices: still no blank page, both appendix bodies present", {
  segs <- apa_docx_page_segments(
    apa_docx_document_xml(apa_render_fixture_docx("float-pages-appendix-two")))
  expect_true(all(nzchar(segs)))
  expect_length(grep("UNIQUEAPPENDIXBODY", segs, fixed = TRUE), 1L)
  expect_length(grep("UNIQUEAPPENDIXTWO",  segs, fixed = TRUE), 1L)
})

test_that("figures with an appendix: no blank page, figure off the body page", {
  segs <- apa_docx_page_segments(
    apa_docx_document_xml(
      apa_render_fixture_docx("float-pages-appendix-figures")))
  expect_true(all(nzchar(segs)))
  fig <- grep("UNIQUEFIGUREONE", segs, fixed = TRUE)
  expect_length(fig, 1L)
  expect_false(grepl("UNIQUEBODYMARKER", segs[fig], fixed = TRUE))
})
