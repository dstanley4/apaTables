# Phase 1.6b Task 7: render-based acceptance for revealjs.
#
# Everything before this task tested apa.quarto.revealjs()'s pieces in
# isolation (test-quarto-slide.R). This file renders REAL decks through a
# REAL `quarto render` and reads the result back out, the same discipline
# test-docx-structure.R and the apaquarto acceptance matrix use for their
# destinations.
#
# WHAT THIS FILE CANNOT SEE, and must not pretend to: D46 section 1 is
# binding. A table that overflows a slide and one that fits render IDENTICAL
# HTML -- 13 <tr> either way. The rows are all in the file; only the
# geometry differs, and geometry lives in a browser, not in the rendered
# markup. So nothing here asserts "fits" -- that check needs a browser and
# is recorded as verified by the spike's browser measurement
# (dev/factcheck/phase16b-slides-spike.md), not by this suite. What CAN be
# checked from a render is completeness, structure (the size div closing
# where it should), furniture opt-in, and absence of apaquarto vocabulary
# the deck never loads a filter for.
#
# PHASE 1.9 (D88/D90, 2026-08-06): PowerPoint left the Quarto SLIDE path --
# `apa.quarto.revealjs()` never routes a pptx render through
# `apa_quarto_revealjs_block()` -- and every pptx-specific test that lived
# in this file at the time ("a pptx render actually contains the table
# (D54 regression guard)", "a raw style block never reaches a PowerPoint
# slide", and "pptx gets a real table with spanner labels over the right
# columns") was deleted along with the fixture
# `fixtures/slide-spanners-pptx.qmd` they rendered, on the premise that
# there was nothing left in this package's pptx output worth checking.
#
# D92 REVERSED THAT PREMISE (whole-branch review of Phase 1.9,
# 2026-08-06). A pptx render still gets a table -- D90's render-then-warn
# was never a refusal -- and D92 restored `apa_slide_flatten_spanners()`
# after a real render showed that table's spanner headers came out WRONG,
# not merely unstyled: `alcohol` sat over the `M` of "None" instead of
# spanning six columns. "a pptx render actually contains the table" and
# "pptx gets a real table with spanner labels over the right columns" are
# both restored below (the fixture too), because a claim this specific
# needs a real `quarto render --to pptx` to back it, the same discipline
# D54 established. They now read differently than their pre-1.9 originals:
# the table reaches this destination via `apa_quarto_markdown_block()`
# (R/quarto-table.R, the shared docx/html emitter a pptx render falls
# through to), not via the slide block, because pptx is not a slide
# destination any more (D88).
#
# D54 (2026-08-04): `apa_slide_size_wrap()` put the table inside a fenced
# Div, and pandoc's pptx writer silently dropped a Table nested inside a
# Div (isolated independent of quarto/R/apaTables with bare pandoc). Fixed
# at the time by skipping the wrap-and-CSS step entirely for pptx. Phase
# 1.9 removed the pptx slide path itself, so this exact failure mode
# cannot recur (`apa_quarto_markdown_block()` never wraps a pptx render in
# a Div at all -- its wrap steps are gated on `is_docx` and an explicit
# HTML `font.size`, both FALSE for pptx), which is why the regression
# guard below only re-checks that the table itself is present, not the
# Div-drop mechanism specifically.

skip_on_cran()
skip_if_not(nzchar(Sys.which("quarto")), "quarto is not installed")
apa_skip_if_library_stale()

test_that("a slide table is complete and its size div closes", {
  html <- apa_html_text(apa_render_slide_fixture("slide-text-after", "revealjs"))

  for (v in c("rating", "complaints", "privileges")) {
    expect_match(html, v, fixed = TRUE)
  }
  expect_match(html, 'style="font-size:', fixed = TRUE)

  # THE DEFECT, constructed and watched in D46 section 8: with the fence
  # unclosed, the sentence below the chunk lands INSIDE the styled div and
  # silently inherits the shrunken type.
  #
  # `class` before `style`, matching pandoc's OWN attribute order for a
  # fenced div (measured: `<div class="apa-slide-table" style="...">`, not
  # the reverse) -- the brief's original pattern assumed the opposite order
  # and matched zero divs; watched failing before this fix, per Step 4.
  #
  # `(?s)` is also required and was also missing from the brief's original:
  # a rendered table div spans many lines, and PCRE's `.` does not match a
  # newline without dotall -- without it `.*?` cannot reach the div's own
  # closing tag and the match is length 0 regardless of the attribute order,
  # which is exactly what was observed before this second fix.
  #
  # M5 (Task 7 review): what this assertion can and cannot see. It checks
  # that the CORRECT code's output is well-formed -- exactly one styled div,
  # excluding the trailing sentence. It does NOT reliably catch the
  # "unclosed fence" defect D46 section 8 names: three constructed mutations
  # of `apa_slide_size_wrap()`'s own closing fence all failed to reproduce
  # the leak through THIS pipeline, because knitr wraps the chunk's output in
  # its own `.cell`/`.cell-output-display` divs, whose genuine closing
  # fences self-heal the mutation -- the still-open level is pushed out to
  # `.cell`, which carries no font-size style, so the trailing text is
  # unstyled either way. The leak reproduces only outside knitr's wrapping (a
  # bare `.qmd` with no R chunk); see the Task 7 report.
  styled <- regmatches(
    html, regexpr('(?s)<div class="apa-slide-table" style="font-size:[^>]*>.*?</div>',
                  html, perl = TRUE))
  expect_length(styled, 1L)
  expect_false(grepl("must NOT be shrunk", styled, fixed = TRUE))
  expect_match(html, "must NOT be shrunk", fixed = TRUE)
})

test_that("no apaquarto vocabulary reaches a deck", {
  html <- apa_html_text(apa_render_slide_fixture("slide-plain", "revealjs"))
  # M6 (Task 7 review): a positive control. Without it, this test was three
  # `expect_false()`s and nothing else -- it would pass just as happily on an
  # EMPTY document, which has none of these strings either. Confirming
  # `apa-slide-table` (a string this render legitimately DOES carry) is
  # present first proves the render produced real content for the absence
  # checks below to be checking anything against.
  expect_match(html, "apa-slide-table", fixed = TRUE)
  # `custom-style` is deliberately NOT in this list. An earlier draft had it
  # here, which was backwards: pandoc turns the ApaTableRule fence into
  # <div data-custom-style="ApaTableRule">, and apa_slide_css() is what makes
  # that draw a spanner's cmidrule (D48). Forbidding it would have banned
  # the mechanism the furniture depends on.
  for (leak in c("apa-table-meta", "apa-table-size-", "apa-table-landscape")) {
    expect_false(grepl(leak, html, fixed = TRUE), info = leak)
  }
})

test_that("the deck gets APA's three rules, and no others", {
  # THE DEFECT THE AUTHOR CAUGHT BY LOOKING (D48). revealjs's own theme puts
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

test_that("a pptx render actually contains the table (D54 regression guard; restored D92)", {
  # THE ABSENCE this whole file existed to find and could not, before this
  # test: D54 measured that `apa_slide_size_wrap()`'s Div wrapper made
  # pandoc's pptx writer drop the Table entirely -- title present, table
  # gone, exit 0. `<a:tbl` is the tag PowerPoint's own XML uses to introduce
  # a table; its absence is exactly what shipped. Real cell values are
  # checked too, not just the tag, so a writer that emitted an EMPTY table
  # shell would not satisfy this either.
  #
  # Restored by D92 (whole-branch review of Phase 1.9): this table now
  # reaches pptx via `apa_quarto_markdown_block()` (R/quarto-table.R), not
  # via the slide block D54 originally guarded -- see the file banner.
  xml <- apa_pptx_slide2_xml(
    apa_render_slide_fixture("slide-spanners-pptx", "pptx"))
  expect_match(xml, "<a:tbl", fixed = TRUE)
  for (val in c("60.62", "alcohol", "None")) {
    expect_match(xml, val, fixed = TRUE)
  }
})

test_that("a raw style block never reaches a PowerPoint slide", {
  # Measured (D48): the pptx writer drops a ```{=html} block silently. If it
  # ever stops doing so, a <style> element prints as visible text on the
  # slide -- so this is checked, not assumed.
  #
  # BEFORE D54's original fix this passed VACUOUSLY: `rows` was always
  # empty (the whole table was gone, not merely unstyled), so `flat <-
  # unlist(rows)` was NULL and both `expect_false(any(grepl(...)))` calls
  # below were trivially TRUE regardless of whether a leak existed. The
  # `expect_gte` is what makes the checks after it mean something -- with
  # it, the table is confirmed present BEFORE its contents are scanned for
  # a leak. `apa_quarto_markdown_block()` (the emitter a pptx render uses
  # since Phase 1.9) never composes a CSS block for pptx at all -- it is
  # gated on `is_docx`/HTML, both FALSE here -- so this stays a real
  # absence check, not a vacuous one.
  rows <- apa_pptx_table_rows(
    apa_render_slide_fixture("slide-spanners-pptx", "pptx"))
  expect_gte(length(rows), 1L)
  flat <- unlist(rows)
  # M7 (Task 7 review): `expect_gte(length(rows), 1L)` proves ROWS exist, not
  # that any cell TEXT was extracted -- `flat` can still be `character(0)` if
  # every cell came back empty, and the two absence checks below would go
  # vacuous again for exactly the same reason D54's original gap did.
  expect_true(any(nzchar(flat)))
  expect_false(any(grepl("<style", flat, fixed = TRUE)))
  expect_false(any(grepl("border-collapse", flat, fixed = TRUE)))
})

test_that("the blocks regression table renders complete, at its capped size", {
  # The author's frame of reference (D47). Ten columns, and the widest of
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

  # 14.6pt is what the two-constraint model returns for this table (D47/D50).
  # Pinned because it is the number a reader of the deck actually sees, and
  # because a silent revert to the height-only model would emit a LARGER size
  # here and overflow. MEASURED, Phase 1.11: reverting px <- min(by_height,
  # by_width) to px <- by_height emits 17.06pt and this assertion catches it
  # (dev/factcheck/phase111-mutation/EVIDENCE.md section 2). This comment
  # previously said 22.75pt -- a figure nobody had ever run, and wrong.
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

test_that("pptx gets a real table with spanner labels over the right columns (D92, restored)", {
  # THE DEFECT D92 EXISTS TO FIX, reproduced end-to-end. Task 1 of Phase 1.9
  # deleted `apa_slide_flatten_spanners()` while a pptx render still
  # produced a table -- the whole-branch review then rendered THIS exact
  # fixture and read `ppt/slides/slide2.xml` back:
  #   row0: ['', 'alcohol', '', '', '', '', '', '', '']
  #   row1: ['', 'None', '', '2 Pints', '', '4 Pints', '', 'Marginal', '']
  # `alcohol` sitting in column 2 instead of spanning six, each condition
  # label sitting over its own `M` alone -- byte-for-byte D46 section 5's
  # original defect. This test is the regression guard for that measurement.
  rows <- apa_pptx_table_rows(
    apa_render_slide_fixture("slide-spanners-pptx", "pptx"))
  expect_gte(length(rows), 4L)
  expect_true(all(lengths(rows) == 9L))

  # D46 section 5 measured the DEFECT as row1 = ['', 'None', '', '2 Pints',
  # ...] with each label in the FIRST column of its pair. After flattening,
  # a label sits over the middle of its group -- offset (span - 1) %/% 2
  # columns in from the group's start.
  #
  # M4 (Task 7 review, carried forward): for a 2-WIDE group that offset is
  # 0 -- the SAME first column the old defect also used. "None"/"2
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

  # D55 regression guard, carried forward. The defect this table's
  # flattened "Marginal" label exposed: a flattened header cell (span == 1,
  # in a non-final header row) was measured by NEITHER apa_md_widths()'s
  # main scan (last header row and data rows only) NOR
  # apa_md_span_widen() (span > 1 cells only), so its column came out one
  # character too narrow. Pandoc's grid-table parser desynced on the
  # overflow and the shipped cell text came back reading "Marginal|" -- a
  # stray pipe baked into visible content, not merely a phantom column. A
  # `|` genuinely inside a cell's own text is not possible from any of this
  # fixture's real values, so any occurrence here is exactly this
  # corruption signature, from a width regression anywhere in this path.
  expect_false(any(grepl("|", unlist(rows), fixed = TRUE)))
})

test_that("an explicit font.size reaches the assembled deck end-to-end", {
  # Carried forward from Task 6's review: Task 3's unit test calls
  # apa_slide_font_pt() directly, so a future edit that dropped `font.size`
  # at the CALL SITE inside apa_quarto_revealjs_block() would not be caught
  # anywhere -- this is the only test in the suite that drives the argument
  # through apa.quarto.revealjs() itself and reads the number back out of a
  # real render.
  #
  # REVEALJS ONLY, and that is permanent, not a gap to close later. D54
  # removed pptx's size wrapper entirely: pandoc's pptx writer has no
  # per-table font-size override to receive `font.size` on, so there is no
  # destination-side mechanism left to assert against on that format. Do
  # not "complete" this test by adding a pptx case -- there is nothing there
  # for it to find, by design (see the D54 comment at
  # apa_quarto_revealjs_block() in R/quarto-slide.R).
  html <- apa_html_text(apa_render_slide_fixture("slide-font-size", "revealjs"))
  expect_match(html, "font-size: 8.5pt;", fixed = TRUE)
})
