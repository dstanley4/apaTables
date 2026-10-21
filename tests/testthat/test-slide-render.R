# Render-based acceptance for revealjs.
#
# test-quarto-slide.R tests apa.quarto.revealjs()'s pieces in isolation.
# This file renders REAL decks through a REAL `quarto render` and reads the
# result back out, the same discipline test-docx-structure.R and the
# apaquarto acceptance matrix use for their destinations.
#
# WHAT THIS FILE CANNOT SEE, and must not pretend to: a table that overflows
# a slide and one that fits render IDENTICAL HTML -- 13 <tr> either way. The
# rows are all in the file; only the geometry differs, and geometry lives in
# a browser, not in the rendered markup. So nothing here asserts "fits" --
# that check needs a browser measurement, not this suite. What CAN be
# checked from a render is completeness, structure (the size div closing
# where it should), furniture opt-in, and absence of apaquarto vocabulary
# the deck never loads a filter for.
#
# PowerPoint is not a Quarto SLIDE destination: `apa.quarto.revealjs()`
# never routes a pptx render through `apa_quarto_revealjs_block()`. But a
# pptx render still gets a table -- via `apa_quarto_markdown_block()`
# (R/quarto-table.R, the shared docx/html emitter a pptx render falls
# through to) -- and that table may be unstyled but must never be WRONG.
# Without `apa_slide_flatten_spanners()` its spanner headers come out
# mislabelled: `alcohol` sits over the `M` of "None" instead of spanning six
# columns (measured). The pptx tests below back claims this specific with a
# real `quarto render --to pptx`.
#
# Never wrap a pptx table in a fenced Div: pandoc's pptx writer silently
# drops a Table nested inside a Div (reproduced with bare pandoc,
# independent of quarto/R/apaTables). `apa_quarto_markdown_block()` never
# does -- its wrap steps are gated on `is_docx` and an explicit HTML
# `font.size`, both FALSE for pptx -- which is why the regression guard
# below re-checks that the table itself is present, not the Div-drop
# mechanism specifically.

skip_on_cran()
skip_if_not(nzchar(Sys.which("quarto")), "quarto is not installed")
apa_skip_if_library_stale()

test_that("a slide table is complete and its size div closes", {
  html <- apa_html_text(apa_render_slide_fixture("slide-text-after", "revealjs"))

  for (v in c("rating", "complaints", "privileges")) {
    expect_match(html, v, fixed = TRUE)
  }
  expect_match(html, 'style="font-size:', fixed = TRUE)

  # THE DEFECT: with the fence unclosed, the sentence below the chunk lands
  # INSIDE the styled div and silently inherits the shrunken type.
  #
  # `class` before `style`, matching pandoc's OWN attribute order for a
  # fenced div (measured: `<div class="apa-slide-table" style="...">`, not
  # the reverse) -- a pattern assuming the opposite order matches zero divs.
  #
  # `(?s)` is also required: a rendered table div spans many lines, and
  # PCRE's `.` does not match a newline without dotall -- without it `.*?`
  # cannot reach the div's own closing tag and the match is length 0
  # regardless of the attribute order.
  #
  # What this assertion can and cannot see. It checks that the CORRECT
  # code's output is well-formed -- exactly one styled div, excluding the
  # trailing sentence. It does NOT reliably catch the "unclosed fence"
  # defect described above: three constructed mutations of
  # `apa_slide_size_wrap()`'s own closing fence all failed to reproduce the
  # leak through THIS pipeline, because knitr wraps the chunk's output in
  # its own `.cell`/`.cell-output-display` divs, whose genuine closing
  # fences self-heal the mutation -- the still-open level is pushed out to
  # `.cell`, which carries no font-size style, so the trailing text is
  # unstyled either way. The leak reproduces only outside knitr's wrapping (a
  # bare `.qmd` with no R chunk).
  styled <- regmatches(
    html, regexpr('(?s)<div class="apa-slide-table" style="font-size:[^>]*>.*?</div>',
                  html, perl = TRUE))
  expect_length(styled, 1L)
  expect_false(grepl("must NOT be shrunk", styled, fixed = TRUE))
  expect_match(html, "must NOT be shrunk", fixed = TRUE)
})

test_that("no apaquarto vocabulary reaches a deck", {
  html <- apa_html_text(apa_render_slide_fixture("slide-plain", "revealjs"))
  # A positive control. Without it, this test would be three
  # `expect_false()`s and nothing else -- passing just as happily on an
  # EMPTY document, which has none of these strings either. Confirming
  # `apa-slide-table` (a string this render legitimately DOES carry) is
  # present first proves the render produced real content for the absence
  # checks below to be checking anything against.
  expect_match(html, "apa-slide-table", fixed = TRUE)
  # `custom-style` is deliberately NOT in this list: pandoc turns the
  # ApaTableRule fence into <div data-custom-style="ApaTableRule">, and
  # apa_slide_css() is what makes that draw a spanner's cmidrule.
  # Forbidding it would ban the mechanism the furniture depends on.
  for (leak in c("apa-table-meta", "apa-table-size-", "apa-table-landscape")) {
    expect_false(grepl(leak, html, fixed = TRUE), info = leak)
  }
})

test_that("the deck gets APA's three rules, and no others", {
  # THE DEFECT, found by looking at a rendered deck: revealjs's own theme puts
  # border-bottom on every th and td and none on the last row, so without
  # apa_slide_css() the table has a rule under every body row and none at
  # its top or bottom -- APA's furniture almost exactly inverted.
  html <- apa_html_text(apa_render_slide_fixture("slide-plain", "revealjs"))

  expect_match(html, "<style>", fixed = TRUE)
  expect_match(html, "apa-slide-table", fixed = TRUE)
  expect_match(html, "border-bottom: none", fixed = TRUE)
  expect_match(html, "vertical-align: bottom", fixed = TRUE)

  # The style block must be INSIDE the deck, not escaped into visible text
  # -- a raw block that pandoc failed to recognise would print as source on
  # the slide, which is the loudest possible way to get this wrong.
  expect_false(grepl("&lt;style&gt;", html, fixed = TRUE))
})

test_that("a pptx render actually contains the table", {
  # A Div wrapper around the table makes pandoc's pptx writer drop the Table
  # entirely -- title present, table gone, exit 0 (measured). `<a:tbl` is
  # the tag PowerPoint's own XML uses to introduce a table; its absence is
  # exactly that failure. Real cell values are checked too, not just the
  # tag, so a writer that emitted an EMPTY table shell would not satisfy
  # this either.
  #
  # This table reaches pptx via `apa_quarto_markdown_block()`
  # (R/quarto-table.R), not via the slide block -- see the file banner.
  xml <- apa_pptx_slide2_xml(
    apa_render_slide_fixture("slide-spanners-pptx", "pptx"))
  expect_match(xml, "<a:tbl", fixed = TRUE)
  for (val in c("60.62", "alcohol", "None")) {
    expect_match(xml, val, fixed = TRUE)
  }
})

test_that("a raw style block never reaches a PowerPoint slide", {
  # Measured: the pptx writer drops a ```{=html} block silently. If it
  # ever stops doing so, a <style> element prints as visible text on the
  # slide -- so this is checked, not assumed.
  #
  # The `expect_gte` is what makes the checks after it mean something: if
  # the whole table were dropped, `rows` would be empty, `flat <-
  # unlist(rows)` NULL, and both `expect_false(any(grepl(...)))` calls
  # below trivially TRUE regardless of whether a leak existed. With it, the
  # table is confirmed present BEFORE its contents are scanned for a leak.
  # `apa_quarto_markdown_block()` (the emitter a pptx render uses) never
  # composes a CSS block for pptx at all -- it is gated on `is_docx`/HTML,
  # both FALSE here -- so this stays a real absence check, not a vacuous
  # one.
  rows <- apa_pptx_table_rows(
    apa_render_slide_fixture("slide-spanners-pptx", "pptx"))
  expect_gte(length(rows), 1L)
  flat <- unlist(rows)
  # `expect_gte(length(rows), 1L)` proves ROWS exist, not that any cell TEXT
  # was extracted -- `flat` can still be `character(0)` if every cell came
  # back empty, and the two absence checks below would go vacuous again.
  expect_true(any(nzchar(flat)))
  expect_false(any(grepl("<style", flat, fixed = TRUE)))
  expect_false(any(grepl("border-collapse", flat, fixed = TRUE)))
})

test_that("the blocks regression table renders complete, at its capped size", {
  # The priority case for slides. Ten columns, and the widest of
  # the common cases -- so it is the one whose size comes from the wrap cap
  # rather than the height budget.
  html <- apa_html_text(apa_render_slide_fixture("slide-plain", "revealjs"))

  # Every block, every predictor, and BOTH fit rows survive.
  for (bit in c("Model 1", "Model 2", "adverts", "airplay", "attract")) {
    expect_match(html, bit, fixed = TRUE)
  }
  # The blocks table's extra column is the change-in-fit one: if it were
  # dropped, everything above would still pass.
  expect_match(html, "Fit", fixed = TRUE)

  # 14.6pt is what the two-constraint model (height budget and wrap cap)
  # returns for this table. Pinned because it is the number a reader of the
  # deck actually sees, and because a silent revert to the height-only model
  # would emit a LARGER size here and overflow. Measured: reverting
  # px <- min(by_height, by_width) to px <- by_height emits 17.06pt and this
  # assertion catches it.
  expect_match(html, "font-size: 14.6pt;", fixed = TRUE)
})

test_that("furniture is opt-in and appears where asked", {
  html <- apa_html_text(apa_render_slide_fixture("slide-furniture", "revealjs"))
  # Slide with title+note carries both; the bare slide carries neither.
  secs <- strsplit(html, "<section", fixed = TRUE)[[1]]
  with_furn <- grep("furniture-on", secs, value = TRUE)
  bare      <- grep("furniture-off", secs, value = TRUE)
  expect_length(with_furn, 1L)
  expect_length(bare, 1L)
  # "Descriptive Statistics and Correlations" is the generated title and
  # "confidence interval" is in the generated note; neither string occurs in
  # a correlation table's own cells, which is why they are the anchors.
  expect_match(with_furn, "Descriptive Statistics and Correlations",
               fixed = TRUE)
  expect_match(with_furn, "confidence interval", fixed = TRUE)
  expect_false(grepl("Descriptive Statistics and Correlations", bare,
                     fixed = TRUE))
  expect_false(grepl("confidence interval", bare, fixed = TRUE))
})

test_that("pptx gets a real table with spanner labels over the right columns", {
  # THE DEFECT, reproduced end-to-end. Without `apa_slide_flatten_spanners()`
  # a pptx render of THIS exact fixture reads `ppt/slides/slide2.xml` back as:
  #   row0: ['', 'alcohol', '', '', '', '', '', '', '']
  #   row1: ['', 'None', '', '2 Pints', '', '4 Pints', '', 'Marginal', '']
  # `alcohol` sitting in column 2 instead of spanning six, each condition
  # label sitting over its own `M` alone. This test is the regression guard
  # for that measurement.
  rows <- apa_pptx_table_rows(
    apa_render_slide_fixture("slide-spanners-pptx", "pptx"))
  expect_gte(length(rows), 4L)
  expect_true(all(lengths(rows) == 9L))

  # The DEFECT measures as row1 = ['', 'None', '', '2 Pints', ...] with
  # each label in the FIRST column of its pair. After flattening,
  # a label sits over the middle of its group -- offset (span - 1) %/% 2
  # columns in from the group's start.
  #
  # For a 2-WIDE group that offset is 0 -- the SAME first column the old
  # defect also used. "None"/"2
  # Pints"/"4 Pints"/"Marginal" are all 2-column groups (M, SD), so the
  # assertion below does not by itself tell flattened output from the old
  # defect apart; it still checks something real (the label survived
  # flattening and sits over the right group), just not the placement
  # change. "alcohol" spans SIX columns (offset 2, landing at column 4 of
  # 9) -- a position the first-column defect could never produce -- so
  # THAT assertion is what actually discriminates.
  expect_identical(rows[[2]][2], "None")
  expect_identical(rows[[1]][4], "alcohol")
  # the body still carries its numbers
  expect_true(any(vapply(rows, function(r) any(grepl("60.62", r, fixed = TRUE)),
                         logical(1))))

  # Width regression guard. The defect this table's flattened "Marginal"
  # label exposed: a flattened header cell (span == 1, in a non-final header
  # row) was measured by NEITHER apa_md_widths()'s main scan (last header
  # row and data rows only) NOR apa_md_span_widen() (span > 1 cells only),
  # so its column came out one
  # character too narrow. Pandoc's grid-table parser desynced on the
  # overflow and the shipped cell text came back reading "Marginal|" -- a
  # stray pipe baked into visible content, not merely a phantom column. A
  # `|` genuinely inside a cell's own text is not possible from any of this
  # fixture's real values, so any occurrence here is exactly this
  # corruption signature, from a width regression anywhere in this path.
  expect_false(any(grepl("|", unlist(rows), fixed = TRUE)))
})

test_that("an explicit font.size reaches the assembled deck end-to-end", {
  # The unit test calls apa_slide_font_pt() directly, so a future edit that
  # dropped `font.size` at the CALL SITE inside apa_quarto_revealjs_block()
  # would not be caught
  # anywhere -- this is the only test in the suite that drives the argument
  # through apa.quarto.revealjs() itself and reads the number back out of a
  # real render.
  #
  # REVEALJS ONLY, and that is permanent, not a gap to close later. pptx has
  # no size wrapper: pandoc's pptx writer has no per-table font-size
  # override to receive `font.size` on, so there is no destination-side
  # mechanism to assert against on that format. Do not "complete" this test
  # by adding a pptx case -- there is nothing there for it to find, by
  # design (see the comment at apa_quarto_revealjs_block() in
  # R/quarto-slide.R).
  html <- apa_html_text(apa_render_slide_fixture("slide-font-size", "revealjs"))
  expect_match(html, "font-size: 8.5pt;", fixed = TRUE)
})
