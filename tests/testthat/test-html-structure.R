# Whole-branch-review Important 1 (2026-08-03): apa.quarto.html() had no
# rendered fixture anywhere in the suite -- every HTML assertion up to this
# point was a unit test with apa_latex_target() mocked, never a real `quarto
# render` to `apaquarto-html`. That gap is what hid Critical 1: the docx-only
# shrink ladder's consequences (an inert `apa-table-size-N` marker div, and a
# warning claiming a floor HTML never applies) were reaching real HTML output
# with no test anywhere positioned to see it.
#
# Mirrors the discipline test-docx-structure.R already uses: a real `quarto
# render`, read the OUTPUT back, never the source .qmd. `expect_false(grepl(
# "apa-table-size-", html))` only became a REAL assertion once Critical 1's
# fix landed in R/quarto-table.R -- it was checked against the pre-fix code
# first and failed there (see the fix report for that evidence).

skip_on_cran()
skip_if_not(nzchar(Sys.which("quarto")), "quarto not installed")

# The spawned quarto R session runs library(apaTables) against the INSTALLED
# package, not this source tree -- see helper-stale-library.R. A green test
# rendered against a stale library certifies the library, not the branch.
apa_skip_if_library_stale()

test_that("a wide table on a real HTML render carries no docx-only shrink-ladder marker or warning, D37", {
  # tbl-wide20 (fixtures/html-wide-table.qmd) is the same shape of table the
  # whole-branch review measured live against format: apaquarto-html: 20
  # variables, wide enough that the automatic ladder would rotate-then-
  # shrink it on a docx render. HTML has no page (D37), so nothing rotates
  # and nothing shrinks -- neither the marker div nor its warning text
  # belongs in this output at all.
  html <- apa_html_text(apa_render_fixture_html("html-wide-table"))

  # No apa-table-size-N div anywhere -- apatablesize.lua stands down for any
  # non-docx FORMAT (:59) and no `apa-table-size-*` CSS rule exists in
  # apa.css, so the div would be dead weight even if it were emitted.
  # Digit-anchored (matching apatablesize.lua's own marker pattern,
  # `^apa%-table%-size%-(%d+)$`), not a bare substring: this fixture's own
  # prose above mentions the marker mechanism by name, and a bare substring
  # check would false-positive on that sentence rather than on a genuinely
  # emitted div.
  expect_false(grepl("apa-table-size-[0-9]", html))

  # No trace of the floor-warning wording apa_quarto_warn_floored() emits,
  # and no Word-only vocabulary ("table styles") leaking into HTML at all.
  expect_false(grepl("the smallest of the table styles", html, fixed = TRUE))
  expect_false(grepl("automatic sizing can choose from", html, fixed = TRUE))

  # The table itself, and its automatic title and note (tbl-cap/apa-note are
  # both deliberately absent from the fixture -- see its own banner), are
  # present: the fix must not have achieved "no warning" by achieving "no
  # table" instead.
  expect_true(grepl("<table", html, fixed = TRUE))
  expect_true(grepl("Var1", html, fixed = TRUE))
  # The generated title names the object (apa.cor.table()'s own generated
  # caption); the generated note is the standard correlation-table note
  # (R/apaCorrelationTable.R:354-360) -- checked on wording that survives
  # HTML's own markup of "N" and "p" (<em>N</em>&nbsp;=, not a plain "N =")
  # untouched.
  expect_true(grepl("Correlations", html, fixed = TRUE))
  expect_true(grepl("number of cases", html, fixed = TRUE))
  expect_true(grepl("indicates", html, fixed = TRUE))
})

# --------------------------------------------------------------------------
# Phase 1.18 / D114 -- the wrapper the table-width rules are scoped to.
#
# These are STRUCTURE assertions: the wrapper exists, there is one of it, it is
# the table's parent, and it does not reach a table the reader wrote. They say
# NOTHING about whether the table actually stopped wrapping -- an unwrapped
# table and a squashed one are IDENTICAL HTML, which is the blindness D46
# recorded for slides. The geometry lives in test-html-width.R, in a browser.
# --------------------------------------------------------------------------

test_that("an HTML render wraps the table in exactly one apa-table-html div, D114", {
  html <- apa_html_text(apa_render_fixture_html("html-wide-table"))

  # the MARKER is consumed -- R emits `::: {.apa-table-html}` as an empty div
  # after the table and apatablesize.lua replaces it with the wrapper. An
  # unconsumed marker prints as an empty div in the page.
  expect_false(grepl("apa-table-size-pt", html, fixed = TRUE))

  # one wrapper, one table
  n_wrap  <- length(gregexpr('class="apa-table-html"', html, fixed = TRUE)[[1]])
  n_table <- length(gregexpr("<table", html, fixed = TRUE)[[1]])
  expect_equal(n_wrap, n_table)

  # and the wrapper is the table's PARENT with nothing in between: apaquarto's
  # APA rules are child selectors on the <table> (`.table > thead`, and
  # `.table > :not(caption) > * > *`), and Phase 1.18's own rules are too
  # (`.apa-table-html > table.table`), so anything inserted between the wrapper
  # and the table silently unstyles the table.
  expect_match(html, '<div class="apa-table-html">\\s*<table')
})

test_that("an explicit font.size still lands on ONE div carrying both classes, D73 + D114", {
  # fixtures/font-size-html.qmd renders a sized table AND a default one, so
  # this exercises both arms of the filter's branch in a single document.
  html <- apa_html_text(apa_render_fixture_html("font-size-html"))

  # BOTH classes on the SAME div. Nesting them instead would leave
  # `.apa-table-size > table` unmatched and hand the table Bootstrap's .small
  # -- 0.875 of the size asked for, silently. That is D73's measurement, and
  # it is the defect this assertion exists to catch.
  expect_match(html, 'class="apa-table-html apa-table-size"')
  expect_match(html, 'style="font-size: 8.5pt;"', fixed = FALSE)

  # no wrapper inside a wrapper, either order
  expect_false(grepl('class="apa-table-html"[^>]*>\\s*<div class="apa-table-size"', html))
  expect_false(grepl('class="apa-table-size"[^>]*>\\s*<div class="apa-table-html"', html))

  # the UNSIZED table in the same document gets the width wrapper and no size
  expect_match(html, '<div class="apa-table-html">\\s*<table')
})

test_that("a table the reader built themselves is not wrapped, D114", {
  # fixtures/html-kable-beside.qmd: one apa.quarto.html() table and one
  # knitr::kable() table. The width rules are scoped to the wrapper, so this
  # assertion IS the scoping: rule 1 overrides column proportions, and a table
  # someone else wrote may carry proportions they chose deliberately.
  html <- apa_html_text(apa_render_fixture_html("html-kable-beside"))

  expect_equal(length(gregexpr('class="apa-table-html"', html, fixed = TRUE)[[1]]), 1L)
  expect_equal(length(gregexpr("<table", html, fixed = TRUE)[[1]]), 2L)

  # both tables really rendered -- "one wrapper" must not be reached by the
  # kable table failing to render at all
  expect_true(grepl("A table the reader built", html, fixed = TRUE))
  expect_true(grepl("complaints", html, fixed = TRUE))
})
