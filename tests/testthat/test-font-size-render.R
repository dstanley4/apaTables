# Rendered evidence for font.size, on real Quarto renders.
#
# Emission is unit-tested in test-font-size-contract.R. This file is the
# other half: a REAL `quarto render`, reading the OUTPUT back, never the
# source .qmd -- the discipline test-docx-structure.R and
# test-html-structure.R already use.

skip_on_cran()
skip_if_not(nzchar(Sys.which("quarto")), "quarto not installed")

# The spawned quarto R session runs library(apaTables) against the INSTALLED
# package, not this source tree -- see helper-stale-library.R. A green test
# rendered against a stale library certifies the library, not the branch.
apa_skip_if_library_stale()

# gregexpr()'s "no match" sentinel is a length-1 vector holding -1, the SAME
# length as a genuine single match -- `length(gregexpr(p, x)[[1]])` cannot
# tell "zero matches" from "one match" apart. This helper counts real
# matches only, treating the -1 sentinel as zero. Proven below
# by a synthetic mutation before it is trusted against rendered output.
apa_count_matches <- function(pattern, text) {
  m <- gregexpr(pattern, text)[[1]]
  if (length(m) == 1L && m[1] == -1L) return(0L)
  length(m)
}

# Reads the SHIPPED extension's apa.css via system.file(), not the source
# tree -- the same discipline test-word-size-styles.R's
# apa_shipped_table_styles() uses, and for the same reason: this certifies
# what a user's INSTALLED package actually carries, not what the source
# tree happens to have. Returns "" (which no assertion below can match) if
# the file is missing, mirroring that helper's own empty-string/skip shape.
apa_shipped_apa_css <- function() {
  css <- system.file("_extensions", "wjschne", "apaquarto", "apa.css",
                     package = "apaTables")
  if (!nzchar(css) || !file.exists(css)) return("")
  paste(readLines(css, warn = FALSE), collapse = "\n")
}

test_that("apa_count_matches distinguishes zero matches from one", {
  expect_equal(apa_count_matches("font-size:", "no size here"), 0L)
  expect_equal(apa_count_matches("font-size:", "one font-size: 8pt"), 1L)
  expect_equal(
    apa_count_matches("font-size:", "font-size: 8pt and font-size: 9pt"), 2L)
})

# Quarto's HTML pipeline was MEASURED to discard a Table node's own
# attributes before the page is written -- at both post-quarto and
# pre-ast, for both a labelled float and an unlabelled table. Setting
# `style=` directly on the <table> survives inside apatablesize.lua's own
# AST but never reaches the rendered page, so do not go back to it. The
# mechanism instead wraps the table in
# `<div class="apa-table-size" style="font-size: Npt;">`, built at
# post-quarto (after apaquarto's float collection, so placement is
# untouched), with a companion CSS rule
# (`.apa-table-size > table { font-size: inherit; }`) that outranks
# Bootstrap's `.small` (which otherwise takes 0.875 of any inherited size)
# so the table takes the wrapper's size verbatim.
#
# What these assertions CANNOT prove: that the table's COMPUTED size on
# screen is actually 8.5pt. Only a browser's layout engine knows that --
# `font-size: inherit` combined with CSS specificity is exactly the kind of
# thing a string match on the markup cannot verify, and a competing rule of
# equal or higher specificity added anywhere else in apa.css later could
# silently defeat this one while every assertion below stays green. The
# on-screen size was verified once by live browser measurement (8.5
# requested -> 8.500pt delivered). A green suite here is evidence
# the MARKUP is right, not proof of the RENDERED size; do not conflate them.
test_that("an explicit font.size reaches an HTML table exactly, via the wrapper div", {
  html <- apa_html_text(apa_render_fixture_html("font-size-html"))

  # 1. the wrapper carries the exact requested size, fractional and all --
  # a rounding regression (8.5 -> 8 or 9) would show here.
  #
  # The wrapper carries TWO classes -- `apa-table-html` (what the
  # table-width rules are scoped to, present on every apaTables table in an
  # HTML render) and `apa-table-size` beside it when a size was asked for.
  # Do not pin the literal `class="apa-table-size"`: that breaks whenever
  # the class list changes while the size is still delivered correctly.
  # These match the class list loosely and the SIZE exactly, which is the
  # property that matters; the
  # class-list SHAPE is asserted once, in test-html-structure.R, where the
  # single-div requirement lives.
  expect_match(html,
    '<div class="[^"]*apa-table-size[^"]*" style="font-size: 8\\.5pt;">',
    fixed = FALSE)

  # 2. the wrapper is IMMEDIATELY around the table -- only whitespace
  # between the wrapper's opening tag and `<table`. If the wrapper drifts
  # to some other position (e.g. around the whole float, or around the
  # caption too) the CSS rule's `>` child combinator stops matching and the
  # size silently stops applying; an assertion that only checked "a wrapper
  # exists somewhere in the document" would not notice that.
  expect_match(html,
    '<div class="[^"]*apa-table-size[^"]*" style="font-size: 8\\.5pt;">\\s*<table')

  # 3. the SHIPPED apa.css carries the companion rule, read via
  # system.file() the same way test-word-size-styles.R reads the Word
  # styles -- so this certifies what a user actually installs, not just
  # what the source tree happens to contain. This is the assertion that
  # matters most: without the rule the mechanism does not fail loudly, it
  # silently delivers 0.875 of the requested size (8.5pt -> 7.4375pt) and
  # every other assertion here would still be green.
  css <- apa_shipped_apa_css()
  expect_match(css,
    '\\.apa-table-size\\s*>\\s*table\\s*\\{\\s*font-size:\\s*inherit;?\\s*\\}')

  # 4. exactly one sized wrapper in the whole document -- the DEFAULT table
  # carries none: nothing shrinks automatically on HTML. Uses
  # apa_count_matches(), not a bare gregexpr() length (see that helper's
  # own comment for why the bare form is unsound).
  expect_equal(apa_count_matches('style="font-size: 8.5pt;"', html), 1L)
  # ... and the OTHER table in the same document does carry the width wrapper,
  # so "exactly one sized wrapper" is not being reached by there being only one
  # wrapper of any kind
  expect_equal(apa_count_matches('class="apa-table-html"', html), 1L)

  # 5. no marker div survives into the output, whatever mechanism consumed it
  expect_false(grepl("apa-table-size-pt", html, fixed = TRUE))
  expect_false(grepl("apa-table-size-[0-9]", html))

  # and Word's range warning must NOT have leaked into an HTML render
  expect_false(grepl("takes the type size of a table from a style", html, fixed = TRUE))
})

test_that("the new Word rungs are selected on a real docx render", {
  docx <- apa_render_fixture_docx("font-size-docx")
  xml  <- apa_docx_document_xml(docx)

  # apa_docx_table_styles() returns ONE entry per <w:tbl>, in document order,
  # which is stronger than grepping the whole XML: it pins WHICH table got
  # which style, so a filter that applied one marker to every table (a
  # LAST-MARKER-WINS defect this filter has had before) fails here
  # instead of passing on "Table11 appears somewhere".
  styles <- apa_docx_table_styles(xml)

  # fixture order: tbl-eleven (11), tbl-twelve (12), tbl-over (16 -> 12)
  expect_identical(styles, c("Table11", "Table12", "Table12"))

  # no style outside Table6-Table12 is ever named -- Word drops an unknown
  # table style entirely, taking the APA rules and cell margins with it
  expect_true(all(styles %in% paste0("Table", 6:12)))
})
