# Markdown tables whose labels share a first part after "tbl-" number 1, 2,
# 3, ... in an apaquarto Word or HTML document, and their cross-references
# match.
#
# apaquarto's markdowntable.lua (it runs for every format except PDF) read a
# markdown table's label with the Lua pattern tbl%-%w+, which stops at the
# first hyphen, underscore, dot or colon after "tbl-". {#tbl-md-plain},
# {#tbl-md-note} and {#tbl-md_under} all became "tbl-md", and the tables
# were captioned alike ("Table 1", "Table 1"; after nine apaTables tables,
# "Table 10", "Table 10"). Labels with distinct first parts (tbl-one,
# tbl-two) were never affected, which is why simpler fixtures numbered
# correctly. Stock apaquarto 5.0.18 does the same.
#
# Mutation, run by hand with the old markdowntable.lua swapped into the
# rendered project (2026-10-22): Word captions and cross-references read
# 1 2 2 2, HTML captions 1 1 1, and both tests fail.

skip_on_cran()
skip_if_not(nzchar(Sys.which("quarto")), "quarto not installed")
apa_skip_if_library_stale()

apa_docx_caption_numbers <- function(doc) {
  caps <- regmatches(doc, gregexpr(
    '<w:pStyle w:val="FigureTitle" /></w:pPr>.*?</w:p>', doc, perl = TRUE))[[1]]
  vapply(caps, function(p) {
    txt <- paste(regmatches(p, gregexpr("<w:t[^>]*>[^<]*</w:t>", p))[[1]],
                 collapse = "")
    gsub("<[^>]+>", "", txt)
  }, character(1), USE.NAMES = FALSE)
}

test_that("hyphenated markdown table labels keep their own numbers in Word", {
  d <- apa_render_fixture_docx("markdown-hyphen-labels-docx")
  doc <- apa_docx_document_xml(d)

  caps <- gsub("\u00a0", " ", apa_docx_caption_numbers(doc))
  expect_identical(caps, paste("Table", 1:4))

  # the references in the first paragraph, in order
  links <- regmatches(doc, gregexpr(
    '<w:hyperlink w:anchor="tbl-[^"]+">.*?</w:hyperlink>', doc, perl = TRUE))[[1]]
  link_text <- gsub("\u00a0", " ", gsub("<[^>]+>", "", links))
  expect_identical(link_text, paste("Table", 1:4))

  # The anchors come from the references in the text, so they read the
  # whole label whatever the filter does; what the defect changed is the
  # table's own identifier, which Word writes as a bookmark. Cut short, all
  # three markdown tables were bookmarked "tbl-md".
  anchors <- sub('.*w:anchor="([^"]+)".*', "\\1", links)
  expect_identical(anchors, c("tbl-apa-first", "tbl-md-plain", "tbl-md-note",
                              "tbl-md_under"))
  marks <- unique(sub('.*w:name="([^"]+)".*', "\\1", regmatches(doc, gregexpr(
    '<w:bookmarkStart [^>]*w:name="tbl-[^"]+"', doc, perl = TRUE))[[1]]))
  expect_setequal(marks, c("tbl-apa-first", "tbl-md-plain", "tbl-md-note",
                           "tbl-md_under"))
})

test_that("hyphenated and underscored markdown table labels keep their own numbers in HTML", {
  h <- apa_render_fixture_html("markdown-hyphen-labels-html")
  html <- paste(readLines(h, warn = FALSE), collapse = "\n")

  caps <- regmatches(html, gregexpr(
    'data-custom-style="FigureTitle">\\s*<p>Table&nbsp;[0-9]+', html, perl = TRUE))[[1]]
  expect_identical(sub(".*Table&nbsp;", "", caps), c("1", "2", "3"))

  links <- regmatches(html, gregexpr(
    '<a href="#tbl-[^"]+" class="quarto-xref"[^>]*>Table&nbsp;[0-9]+</a>',
    html, perl = TRUE))[[1]]
  expect_identical(sub('.*href="#([^"]+)".*', "\\1", links),
                   c("tbl-md-plain", "tbl-md-note", "tbl-md_under"))
  expect_identical(sub(".*Table&nbsp;([0-9]+)</a>", "\\1", links),
                   c("1", "2", "3"))

  # the tables' own ids, which the defect cut short to "tbl-md"
  expect_false(grepl('id="tbl-md"', html, fixed = TRUE))
  for (id in c("tbl-md-plain", "tbl-md-note", "tbl-md_under")) {
    expect_true(grepl(paste0('id="', id, '"'), html, fixed = TRUE), info = id)
  }
})

test_that("a panel of markdown subtables still renders and numbers as one table", {
  # Subtables labelled from the panel's own label ({#tbl-pair} holding
  # {#tbl-pair-a} and {#tbl-pair-b}) are read as before inside the panel,
  # which gives them the panel's identifier: the panel is Table 1 and the
  # table after it Table 2. Reading their whole labels numbered the panel
  # 3 and the next table 4 against references reading 1 and 4; leaving
  # them without identifiers stopped the render.
  d <- apa_render_fixture_docx("markdown-subtable-panel-docx")
  doc <- apa_docx_document_xml(d)
  caps <- gsub("\u00a0", " ", apa_docx_caption_numbers(doc))
  expect_identical(caps, c("Table 1", "Table 2"))

  links <- regmatches(doc, gregexpr(
    '<w:hyperlink w:anchor="tbl-(pair|after)">.*?</w:hyperlink>', doc, perl = TRUE))[[1]]
  expect_identical(gsub("\u00a0", " ", gsub("<[^>]+>", "", links)),
                   c("Table 1", "Table 2"))
})
